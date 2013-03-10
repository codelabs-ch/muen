-----------------------------------------------------------------------
--  Disassembler -- Disassembler
--  <!-- Copyright (C) 2003, 2004, 2006, 2012 Free Software Foundation, Inc.
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
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
--  The <tt>Bfd.Disassembler</tt> package exports the dissassembler API
--  of the GNU opcodes library found in Binutils.
--
with System;
with Ada.Streams;

with Bfd.Files;
with Bfd.Symbols;
package Bfd.Disassembler is

   type Insn_Type is (NONINSN,          --  Not a valid instruction
                      NONBRANCH,        --  Not a branch instruction
                      BRANCH,           --  Unconditional branch
                      CONDBRANCH,       --  Conditional branch
                      JSR,              --  Jump to subroutine
                      CONDJSR,          --  Conditional jump to subroutine
                      DREF,             --  Data reference instruction
                      DREF2);           --  Two data references in instruction

   type Insn_Info_Type is record
      Insn        : Insn_Type;  --  Type of instruction
      Target      : Vma_Type;   --  Target address of branch/dref if known
      Target2     : Vma_Type;   --  Second target address for dref2

      --  How many sequential insn's will run before a branch takes effect.
      Delay_Insns : Natural;
      Data_Size   : Natural;    --  Size of data reference in insn (in bytes)
   end record;

   ----------------------
   --  Disassembler_Info_Type root type
   ----------------------
   type Disassembler_Info_Type is abstract tagged private;

   --  Initialize the disassembler according to the BFD file.
   procedure Initialize (Info     : in out Disassembler_Info_Type'Class;
                         For_File : in Bfd.Files.File_Type;
                         Options  : in String);

   --  Disassemble one instruction at address Addr.
   --  It uses the Read procedure to read the memory to disassemble
   --  and calls Output to print the result.  Once disassembled,
   --  the address of the next instruction is returned in Next_Addr.
   procedure Disassemble (Info      : in out Disassembler_Info_Type;
                          Addr      : in Vma_Type;
                          Next_Addr : out Vma_Type);

   --  Print the result of disassembling the current instruction.
   --  This procedure is called by the disassembler to print the instruction
   --  and its operands.
   procedure Output (Info : in out Disassembler_Info_Type;
                     Item : in String) is abstract;

   --  Print the address.
   --  The default just translates the address in hexadecimal and
   --  prints it with the Output procedure.  It can be overriden by a
   --  derived type to print a symbolic address.
   procedure Output (Info : in out Disassembler_Info_Type;
                     Addr : in Vma_Type);

   --  Returns true if there is a symbol at the given address.
   --  The default always returns true.
   function Symbol_At (Info : in Disassembler_Info_Type;
                       Addr : in Vma_Type) return Boolean;

   --  Reads Item'Size bytes starting at the given address.
   --  This procedure is called by the disassembler to obtain the memory
   --  to disassemble.  It is called several times depending on the assembler.
   procedure Read (Info : in out Disassembler_Info_Type;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset) is abstract;

   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.
   procedure Memory_Error (Info : in out Disassembler_Info_Type;
                           Addr : in Vma_Type) is abstract;

   --  Set the symbol table associated with the disassembler.
   procedure Set_Symbol_Table (Info   : in out Disassembler_Info_Type;
                               Symtab : in Bfd.Symbols.Symbol_Table);

   --  --------------------
   --  Memory Disassembler type
   --  --------------------
   type Memory_Disassembler_Info_Type is abstract new Disassembler_Info_Type
     with private;

   --  Initialize the disassembler according to the BFD file.
   --  Setup the disassembler to the buffer passed in Buffer for reading
   --  the instructions to disassemble.  The buffer's starting VMA address
   --  is specified by Buffer_Vma.
   procedure Initialize (Info       : in out Memory_Disassembler_Info_Type'Class;
                         For_File   : in Bfd.Files.File_Type;
                         Options    : in String;
                         Buffer_Vma : in Vma_Type;
                         Buffer     : in Ada.Streams.Stream_Element_Array);

   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.
   procedure Memory_Error (Info : in out Memory_Disassembler_Info_Type;
                           Addr : in Vma_Type);

   --  Print the result of disassembling the current instruction.
   --  This procedure is called to print the instruction and its
   --  operands.
   procedure Output (Info : in out Memory_Disassembler_Info_Type;
                     Item : in String) is abstract;

   --  Reads Item'Size bytes starting at the given address.
   --  This procedure uses the buffer passed to Disassembler to obtain
   --  the memory.
   procedure Read (Info : in out Memory_Disassembler_Info_Type;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset);

private

   subtype D is System.Address;

   type Disassembler_Info_Type is abstract tagged record
      Dis_Info : D;
      Abfd     : Ptr := System.Null_Address;
   end record;

   type Memory_Disassembler_Info_Type is abstract new Disassembler_Info_Type
     with null record;

end Bfd.Disassembler;
