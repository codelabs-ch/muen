-----------------------------------------------------------------------
--  bfd-symbols -- BFD Symbol Table types and operations
--  Copyright (C) 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
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
--  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
--  This package gives access to the symbol table managed by the
--  BFD library.
--
with System;
with Ada.Strings.Unbounded;
with Ada.Finalization;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Constants;
package Bfd.Symbols is

   use Ada.Strings.Unbounded;

   ----------------------
   -- Symbol_Flags     --
   ----------------------
   --  Represents the flags associated with a symbol.

   --  Constants below are extracted from the BFD C source file
   --  CODE FRAGMENT:  bfd/syms.c:

   --  Attributes of a symbol.
   BSF_NO_FLAGS : constant Symbol_Flags := Constants.BSF_NO_FLAGS;

   --  The symbol has local scope; <<static>> in <<C>>. The value
   --  is the offset into the section of the data.
   BSF_LOCAL : constant Symbol_Flags := Constants.BSF_LOCAL;

   --  The symbol has global scope; initialized data in <<C>>. The
   --  value is the offset into the section of the data.
   BSF_GLOBAL : constant Symbol_Flags := Constants.BSF_GLOBAL;

   --  The symbol has global scope and is exported. The value is
   --  the offset into the section of the data.
   BSF_EXPORT : constant Symbol_Flags := Constants.BSF_GLOBAL;

   --  A normal C symbol would be one of:
   --  <<BSF_LOCAL>>, <<BSF_COMMON>>,  <<BSF_UNDEFINED>> or
   --  <<BSF_GLOBAL>>.

   --  The symbol is a debugging record. The value has an arbitrary
   --  meaning, unless BSF_DEBUGGING_RELOC is also set.
   BSF_DEBUGGING : constant Symbol_Flags := Constants.BSF_DEBUGGING;

   --  The symbol denotes a function entry point.  Used in ELF,
   --  perhaps others someday.
   BSF_FUNCTION : constant Symbol_Flags := Constants.BSF_FUNCTION;

   --  Used by the linker.
   BSF_KEEP : constant Symbol_Flags := Constants.BSF_KEEP;
   BSF_KEEP_G : constant Symbol_Flags := Constants.BSF_KEEP_G;

   --  A weak global symbol, overridable without warnings by
   --  a regular global symbol of the same name.
   BSF_WEAK : constant Symbol_Flags := Constants.BSF_WEAK;

   --  This symbol was created to point to a section, e.g. ELF's
   --  STT_SECTION symbols.
   BSF_SECTION_SYM : constant Symbol_Flags := Constants.BSF_SECTION_SYM;

   --  The symbol used to be a common symbol, but now it is
   --  allocated.
   BSF_OLD_COMMON : constant Symbol_Flags := Constants.BSF_OLD_COMMON;

   --  In some files the type of a symbol sometimes alters its
   --  location in an output file - ie in coff a <<ISFCN>> symbol
   --  which is also <<C_EXT>> symbol appears where it was
   --  declared and not at the end of a section.  This bit is set
   --  by the target BFD part to convey this information.
   BSF_NOT_AT_END : constant Symbol_Flags := Constants.BSF_NOT_AT_END;

   --  Signal that the symbol is the label of constructor section.
   BSF_CONSTRUCTOR : constant Symbol_Flags := Constants.BSF_CONSTRUCTOR;

   --  Signal that the symbol is a warning symbol.  The name is a
   --  warning.  The name of the next symbol is the one to warn about;
   --  if a reference is made to a symbol with the same name as the next
   --  symbol, a warning is issued by the linker.
   BSF_WARNING : constant Symbol_Flags := Constants.BSF_WARNING;

   --  Signal that the symbol is indirect.  This symbol is an indirect
   --  pointer to the symbol with the same name as the next symbol.
   BSF_INDIRECT : constant Symbol_Flags := Constants.BSF_INDIRECT;

   --  BSF_FILE marks symbols that contain a file name.  This is used
   --  for ELF STT_FILE symbols.
   BSF_FILE : constant Symbol_Flags := Constants.BSF_FILE;

   --  Symbol is from dynamic linking information.
   BSF_DYNAMIC : constant Symbol_Flags := Constants.BSF_DYNAMIC;

   --  The symbol denotes a data object.  Used in ELF, and perhaps
   --  others someday.
   BSF_OBJECT : constant Symbol_Flags := Constants.BSF_OBJECT;

   --  This symbol is a debugging symbol.  The value is the offset
   --  into the section of the data.  BSF_DEBUGGING should be set
   --  as well.
   BSF_DEBUGGING_RELOC : constant Symbol_Flags := Constants.BSF_DEBUGGING_RELOC;

   --  This symbol is thread local.  Used in ELF.
   BSF_THREAD_LOCAL : constant Symbol_Flags := Constants.BSF_THREAD_LOCAL;

   --  This symbol represents a complex relocation expression,
   --  with the expression tree serialized in the symbol name.
   BSF_RELC : constant Symbol_Flags := Constants.BSF_RELC;

   --  This symbol represents a signed complex relocation expression,
   --  with the expression tree serialized in the symbol name.
   BSF_SRELC : constant Symbol_Flags := Constants.BSF_SRELC;

   --  This symbol was created by bfd_get_synthetic_symtab.
   BSF_SYNTHETIC : constant Symbol_Flags := Constants.BSF_SYNTHETIC;

   --  This symbol is an indirect code object.  Unrelated to BSF_INDIRECT.
   --  The dynamic linker will compute the value of this symbol by
   --  calling the function that it points to.  BSF_FUNCTION must
   --  also be also set.
   BSF_GNU_INDIRECT_FUNCTION : constant Symbol_Flags := Constants.BSF_GNU_INDIRECT_FUNCTION;

   --  This symbol is a globally unique data object.  The dynamic linker
   --  will make sure that in the entire process there is just one symbol
   --  with this name and type in use.  BSF_OBJECT must also be set.
   BSF_GNU_UNIQUE : constant Symbol_Flags := Constants.BSF_GNU_UNIQUE;

   --  END FRAGMENT: bfd/syms.c

   ----------------------
   -- Symbol           --
   ----------------------
   type Symbol is private;

   Null_Symbol : constant Symbol;

   function Get_Flags (Sym : in Symbol) return Symbol_Flags;
   --  Get the flags associated with the symbol.

   function Get_Name (Sym : in Symbol) return String;
   --  Return the symbol name.

   function Get_Section (Sym : in Symbol) return Bfd.Sections.Section;
   --  Return the section where the symbol is defined.

   function Get_Value (Sym : in Symbol) return Symbol_Value;
   --  Return the value

   function Get_Symclass (Sym : in Symbol) return Character;
   --  Return a character corresponding to the symbol class of Sym.

   function Is_Local_Label (File : in Bfd.Files.File_Type;
                            Sym  : in Symbol) return Boolean;
   --  Returns true if the symbol is local.

   function Is_Local_Label_Name (File : in Bfd.Files.File_Type;
                                 Name : in String) return Boolean;
   --  Returns true if the label is local.

   function Is_Undefined_Class (C : Character) return Boolean;

   ----------------------
   -- Symbol_Table     --
   ----------------------
   type Symbol_Array is array (Positive range <>) of Symbol;
   type Symbol_Array_Access is access all Symbol_Array;

   type Symbol_Table is limited private;

   type Symbol_Iterator is private;

   function Has_Element (It : Symbol_Iterator) return Boolean;
   --  Return true if the iterator contains an element.

   procedure Next (It : in out Symbol_Iterator);
   --  Move the iterator to the next element.

   function Element (It : in Symbol_Iterator) return Symbol;
   --  Return the current symbol pointed to by the iterator.

   procedure Read_Symbols (File    : in Bfd.Files.File_Type;
                           Symbols : out Symbol_Table);
   --  Open and read all the symbols.

   function Get_Iterator (Symbols : in Symbol_Table) return Symbol_Iterator;
   --  Return an iterator which allows scanning the symbol table.

   procedure Find_Nearest_Line (File : in Bfd.Files.File_Type;
                                Sec : in Bfd.Sections.Section;
                                Symbols : in Symbol_Table;
                                Addr : in Vma_Type;
                                Name : out Unbounded_String;
                                Func : out Unbounded_String;
                                Line : out Natural);
   --  Find the nearest source file and line for a given address.
   --  Equivalent to bfd_find_nearest_line ().

   function Get_Symbol (Symbols : in Symbol_Table;
                        Pos : in Positive) return Symbol;

   --  Get the symbol with the given name.
   --  Returns Null_Symbol if the symbol was not found.
   function Get_Symbol (Symbols : in Symbol_Table;
                        Name    : in String) return Symbol;

   function Get_Size (Symbols : in Symbol_Table) return Natural;

   --  Internal operation to obtain the symbol table for the disassembler.
   function Get_Internal_Symbols (Symbols : in Symbol_Table) return Symbol_Array_Access;

private

   type Symbol is new System.Address;
   --  To avoid memory copies the 'Symbol' is directly mapped to
   --  the BFD asymbol structure.  The C definition is not imported
   --  to simplify things.  The symbol table in BFD is an array
   --  of asymbol pointers (asymbol**).

   Null_Symbol : constant Symbol := Symbol (System.Null_Address);

   type Symbol_Table is new Ada.Finalization.Limited_Controlled with record
      Syms   : Symbol_Array_Access;
      Size   : Natural := 0;
      Sorted : Boolean := False;
   end record;

   --  Release the symbol table.
   overriding
   procedure Finalize (Symbols : in out Symbol_Table);

   type Symbol_Iterator is record
      Syms   : Symbol_Array_Access;
      Size   : Natural := 0;
      Pos    : Positive := 1;
   end record;
   --  The symbol iterator keeps track of the symbol table
   --  and uses an index within it to mark the current symbol.

   pragma Import (C, Get_Symclass, "bfd_decode_symclass");
   --  C Functions provided by BFD library.

   pragma Import (C, Get_Value, "ada_bfd_asymbol_value");
   pragma Import (C, Get_Flags, "ada_bfd_asymbol_flags");
   --  C Functions provided by specific wrapper.

end Bfd.Symbols;
