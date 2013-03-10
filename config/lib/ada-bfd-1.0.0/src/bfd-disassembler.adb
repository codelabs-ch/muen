-----------------------------------------------------------------------
--  Disassembler -- Disassembler
--  Copyright (C) 2003, 2006, 2012 Free Software Foundation, Inc.
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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with Interfaces.C;
use  Interfaces;
with Interfaces.C.Strings;
with Bfd.Thin.Disassembler;
package body Bfd.Disassembler is

   EIO : constant Integer := -3;

   type Disassembler_Ref is access all Disassembler_Info_Type'Class;

   function Get_Disassembler_Data (Data : in Ptr) return Disassembler_Ref;
   pragma Import (C, Get_Disassembler_Data, "bfd_ada_disassembler_get_data");

   ----------------------
   --  Handlers called by C callbacks
   ----------------------
   procedure Disassembler_Output (Info   : Disassembler_Ref;
                                  Buffer : Interfaces.C.Strings.chars_ptr);
   pragma Export (C, Disassembler_Output, "ada_disassembler_output");

   ----------------------
   --  The prototype of these callbacks must match the functions
   --  defined in Binutils 'include/dis-asm.h'.
   ----------------------
   procedure Disassembler_Output_Address (Addr : Vma_Type;
                                          Data : in Ptr);
   pragma Export (C, Disassembler_Output_Address, "ada_disassembler_output_address");

   procedure Memory_Handler (Status : in Integer;
                             Addr   : in Vma_Type;
                             Data   : in Ptr);
   pragma Export (C, Memory_Handler, "ada_dis_memory_handler");

   function Symbol_At_Address (Addr : in Vma_Type;
                               Data : in Ptr) return Interfaces.C.int;
   pragma Export (C, Symbol_At_Address, "ada_dis_symbol_at_address");

   function Read_Memory_Handler (Addr        : in Vma_Type;
                                 Buffer_Addr : in System.Address;
                                 Len         : in Integer;
                                 Data        : in Ptr) return Integer;
   pragma Export (C, Read_Memory_Handler, "ada_dis_read_memory_handler");

   procedure Disassembler_Output (Info   : Disassembler_Ref;
                                  Buffer : Interfaces.C.Strings.chars_ptr) is
   begin
      Output (Info.all, Interfaces.C.Strings.Value (Buffer));
   end Disassembler_Output;

   procedure Disassembler_Output_Address (Addr : in Vma_Type;
                                          Data : in Ptr) is
      Info : constant Disassembler_Ref := Get_Disassembler_Data (Data);
   begin
      Output (Info.all, Addr);
   end Disassembler_Output_Address;

   ----------------------
   --  Memory error callback
   ----------------------
   procedure Memory_Handler (Status : in Integer;
                             Addr   : in Vma_Type;
                             Data   : in Ptr) is
      pragma Unreferenced (Status);

      Info : constant Disassembler_Ref := Get_Disassembler_Data (Data);
   begin
      Memory_Error (Info.all, Addr);
   end Memory_Handler;

   ----------------------
   --  Symbol check callback
   ----------------------
   function Symbol_At_Address (Addr : in Vma_Type;
                               Data : in Ptr) return Interfaces.C.int is
      Info : constant Disassembler_Ref := Get_Disassembler_Data (Data);
   begin
      if Symbol_At (Info.all, Addr) then
         return 1;
      else
         return 0;
      end if;
   end Symbol_At_Address;

   ----------------------
   --  Read memory callback
   ----------------------
   function Read_Memory_Handler (Addr        : in Vma_Type;
                                 Buffer_Addr : in System.Address;
                                 Len         : in Integer;
                                 Data        : in Ptr) return Integer is
      use type Ada.Streams.Stream_Element_Offset;

      --  Buffer_Addr points to some local buffer allocated by the
      --  opcodes C library (on the stack).  Map that buffer to the
      --  Ada type (no copy).
      Buf : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Len));
      for Buf'Address use Buffer_Addr;

      Last : Ada.Streams.Stream_Element_Offset;
      Info : constant Disassembler_Ref := Get_Disassembler_Data (Data);
   begin
      Read (Info.all, Addr, Buf, Last);
      if Last = Buf'Last then
         return 0;
      else
         return EIO;
      end if;
   end Read_Memory_Handler;

   ----------------------
   --  Disassembler_Info_Type methods
   ----------------------
   --------------------------------------------------
   --  Convert an address to a string in hexadecimal form
   --------------------------------------------------
   function HexImage (Addr : in Bfd.Vma_Type) return String;

   function HexImage (Addr : in Bfd.Vma_Type) return String is
      Map : constant String := "0123456789ABCDEF";
      S   : String (1 .. 40);
      Val : Bfd.Vma_Type := Addr;
      C   : Natural;
      Pos : Positive := S'Last;
   begin
      loop
         C := Natural (Val mod 16);
         Val := Val / 16;
         S (Pos) := Map (C + 1);
         exit when Val = 0;
         Pos := Pos - 1;
      end loop;
      return S (Pos .. S'Last);
   end HexImage;

   ----------------------
   --  Print the address.
   --  The default just translated the address in hexadecimal and
   --  prints it with Output procedure.  It can be overriden by a
   --  derived type to print a symbolic address.
   ----------------------
   procedure Output (Info : in out Disassembler_Info_Type;
                     Addr : in Vma_Type) is
   begin
      Output (Disassembler_Info_Type'Class (Info), HexImage (Addr));
   end Output;

   ----------------------
   --  Returns true if there is a symbol at the given address.
   --  The default always returns true.
   ----------------------
   function Symbol_At (Info : in Disassembler_Info_Type;
                       Addr : in Vma_Type) return Boolean is
      pragma Unreferenced (Info, Addr);
   begin
      return True;
   end Symbol_At;

   ----------------------
   --  Initialize the disassembler according to the BFD file.
   ----------------------
   procedure Initialize (Info     : in out Disassembler_Info_Type'Class;
                         For_File : in Bfd.Files.File_Type;
                         Options  : in String) is
      use Bfd.Thin.Disassembler;
   begin
      Info.Abfd := Bfd.Files.Get_Bfd_Pointer (For_File);
      Info.Dis_Info := Disassembler_Init (Info'Address, Info.Abfd,
                                          Interfaces.C.Strings.New_String (Options));
   end Initialize;

   ----------------------
   --  Set the symbol table associated with the disassembler.
   ----------------------
   procedure Set_Symbol_Table (Info   : in out Disassembler_Info_Type;
                               Symtab : in Bfd.Symbols.Symbol_Table) is
      use Bfd.Thin.Disassembler;
   begin
      Set_Symbol_Table (Info.Dis_Info, Bfd.Symbols.Get_Internal_Symbols (Symtab) (1)'Address,
                        Bfd.Symbols.Get_Size (Symtab));
   end Set_Symbol_Table;

   ----------------------
   --  Disassembler one instruction at address Addr.
   --  Use the Read procedure to read the memory to disassemble.
   ----------------------
   procedure Disassemble (Info      : in out Disassembler_Info_Type;
                          Addr      : in Vma_Type;
                          Next_Addr : out Vma_Type) is
      use Bfd.Thin.Disassembler;

      Size : constant Integer := Disassemble (Info.Abfd, Info.Dis_Info, Addr);
   begin
      Next_Addr := Addr + Vma_Type (Size);
   end Disassemble;

   ----------------------
   --  Memory Disassembler_Info_Type type
   ----------------------

   ----------------------
   --  Initialize the disassembler according to the BFD file.
   ----------------------
   procedure Initialize (Info       : in out Memory_Disassembler_Info_Type'Class;
                         For_File   : in Bfd.Files.File_Type;
                         Options    : in String;
                         Buffer_Vma : in Vma_Type;
                         Buffer     : in Ada.Streams.Stream_Element_Array) is
      use Bfd.Thin.Disassembler;
   begin
      Initialize (Info, For_File, Options);
      Set_Buffer (Info.Dis_Info,
                  Buffer (Buffer'First)'Address, Buffer'Size, Buffer_Vma);
   end Initialize;

   ----------------------
   --  Reads Item'Size bytes starting at the given address.
   --  This procedure uses the buffer passed to Disassembler to obtain
   --  the memory.
   ----------------------
   procedure Read (Info  : in out Memory_Disassembler_Info_Type;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error;
   end Read;

   ----------------------
   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.
   ----------------------
   procedure Memory_Error (Info : in out Memory_Disassembler_Info_Type;
                           Addr : in Vma_Type) is
   begin
      null;
   end Memory_Error;

end Bfd.Disassembler;
