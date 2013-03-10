-----------------------------------------------------------------------
--  Symbols -- BFD Symbol Table types and operations
--  Copyright (C) 2002, 2003, 2012 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of BfdAda.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with Ada.Unchecked_Deallocation;

with Bfd.Internal;
with Bfd.Thin.Symbols;
with Interfaces.C;
with Interfaces.C.Strings;
package body Bfd.Symbols is

   use Bfd.Internal;
   use Bfd.Sections;
   use type Interfaces.C.int;

   procedure Free is
     new Ada.Unchecked_Deallocation (Symbol_Array, Symbol_Array_Access);

   ----------------------
   --  Return the symbol name.
   ----------------------
   function Get_Name (Sym : in Symbol) return String is
   begin
      return Interfaces.C.Strings.Value (Bfd.Thin.Symbols.Get_Symbol_Name (Sym));
   end Get_Name;

   ----------------------
   --  Return the section where the symbol is defined.
   ----------------------
   function Get_Section (Sym : in Symbol) return Section is
   begin
      return Element (Bfd.Thin.Symbols.Get_Symbol_Section (Sym));
   end Get_Section;

   ----------------------
   --  Returns true if the symbol is local.
   ----------------------
   function Is_Local_Label (File : in Bfd.Files.File_Type;
                            Sym  : in Symbol) return Boolean is
   begin
      return Bfd.Thin.Symbols.Is_Local (Bfd.Files.Get_Bfd_Pointer (File), Sym) /= 0;
   end Is_Local_Label;

   ----------------------
   --  Returns true if the label is local.
   ----------------------
   function Is_Local_Label_Name (File : in Bfd.Files.File_Type;
                                 Name : in String) return Boolean is
   begin
      --  return Is_Local (File.Abfd, Name & ASCII.NUL);
      return False;
   end Is_Local_Label_Name;

   function Is_Undefined_Class (C : in Character) return Boolean is
      function Bfd_Is_Undefined_Class (C : in Character) return Interfaces.C.int;
      pragma Import (C, Bfd_Is_Undefined_Class, "bfd_is_undefined_symclass");
   begin
      return Bfd_Is_Undefined_Class (C) /= 0;
   end Is_Undefined_Class;

   ----------------------
   --  Symbol table iterator
   ----------------------

   ----------------------
   --  Return true if we are at end of the iterator.
   ----------------------
   function Has_Element (It : Symbol_Iterator) return Boolean is
   begin
      return It.Pos <= It.Size;
   end Has_Element;

   ----------------------
   --  Move the iterator to the next element.
   ----------------------
   procedure Next (It : in out Symbol_Iterator) is
   begin
      It.Pos := It.Pos + 1;
   end Next;

   ----------------------
   --  Return the current symbol pointed to by the iterator.
   ----------------------
   function Element (It : in Symbol_Iterator) return Symbol is
   begin
      return It.Syms (It.Pos);
   end Element;

   ----------------------
   --  Return an iterator which allows scanning the symbol table.
   ----------------------
   function Get_Iterator (Symbols : in Symbol_Table) return Symbol_Iterator is
      It : Symbol_Iterator;
   begin
      It.Syms := Symbols.Syms;
      It.Size := Symbols.Size;
      It.Pos  := 1;
      return It;
   end Get_Iterator;

   ----------------------
   --  Symbol table operations
   ----------------------

   ----------------------
   --  Open and read all the symbols.
   ----------------------
   procedure Read_Symbols (File    : in Bfd.Files.File_Type;
                           Symbols : out Symbol_Table) is

      Cnt : aliased Integer
        := Bfd.Thin.Symbols.Get_Symtab_Upper_Bound (Bfd.Files.Get_Bfd_Pointer (File));

      subtype Symbol_Array_Type is Symbol_Array (1 .. Positive (Cnt));

      Syms : Symbol_Array_Access := new Symbol_Array_Type;
   begin
      Bfd.Thin.Symbols.Read_Symbols (Bfd.Files.Get_Bfd_Pointer (File), Cnt'Address,
                                    Syms (1)'Address);
      if Cnt < 0 then
         Free (Syms);
         raise OPEN_ERROR;
      end if;
      Symbols.Size := Natural (Cnt);
      Symbols.Syms := Syms;
   end Read_Symbols;

   procedure Find_Nearest_Line (File : in Bfd.Files.File_Type;
                                Sec : in Section;
                                Symbols : in Symbol_Table;
                                Addr : Vma_Type;
                                Name : out Unbounded_String;
                                Func : out Unbounded_String;
                                Line : out Natural) is

      use type Interfaces.C.Strings.chars_ptr;

      File_Name : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
      Func_Name : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
      L : aliased Integer := -1;

   begin
      Bfd.Thin.Symbols.Find_Nearest_Line (Bfd.Files.Get_Bfd_Pointer (File), Sec.Opaque,
                                          Symbols.Syms (1)'Address, Addr,
                                          File_Name'Address,
                                          Func_Name'Address,
                                          L'Address);
      if L < 0 then
         raise NOT_FOUND;
      end if;

      Line := Natural (L);
      if File_Name /= Interfaces.C.Strings.Null_Ptr then
         Name := To_Unbounded_String (Interfaces.C.Strings.Value (File_Name));
      else
         Name := To_Unbounded_String ("");
      end if;
      if Func_Name /= Interfaces.C.Strings.Null_Ptr then
         Func := To_Unbounded_String (Interfaces.C.Strings.Value (Func_Name));
      else
         Func := To_Unbounded_String ("");
      end if;
   end Find_Nearest_Line;

   function Get_Symbol (Symbols : in Symbol_Table;
                        Pos : in Positive) return Symbol is
   begin
      return Symbols.Syms (Pos);
   end Get_Symbol;

   --  --------------------
   --  Get the symbol with the given name.
   --  Returns Null_Symbol if the symbol was not found.
   --  --------------------
   function Get_Symbol (Symbols : in Symbol_Table;
                        Name    : in String) return Symbol is
      Syms   : constant Symbol_Array_Access := Symbols.Syms;
      S      : Symbol;
      C_Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Name);
   begin
      if not Symbols.Sorted then
         for I in 1 .. Symbols.Size loop
            S := Syms (I);
            if Bfd.Internal.Strcmp (C_Name, Bfd.Thin.Symbols.Get_Symbol_Name (S)) = 0 then
               Interfaces.C.Strings.Free (C_Name);
               return S;
            end if;
         end loop;
      end if;
      Interfaces.C.Strings.Free (C_Name);
      return Null_Symbol;
   end Get_Symbol;

   function Get_Size (Symbols : in Symbol_Table) return Natural is
   begin
      return Symbols.Size;
   end Get_Size;

   ----------------------
   --  Internal operation to obtain the symbol table for the disassembler.
   ----------------------
   function Get_Internal_Symbols (Symbols : in Symbol_Table) return Symbol_Array_Access is
   begin
      return Symbols.Syms;
   end Get_Internal_Symbols;

   ----------------------
   --  Release the symbol table.
   ----------------------
   overriding
   procedure Finalize (Symbols : in out Symbol_Table) is
   begin
      Free (Symbols.Syms);
   end Finalize;

end Bfd.Symbols;
