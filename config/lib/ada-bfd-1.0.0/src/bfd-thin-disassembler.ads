-----------------------------------------------------------------------
--  BFD -- Thin Ada layer for Bfd disassembler (common Bfd functions)
--  <!-- Copyright (C) 2002, 2003, 2004, 2006, 2012 Free Software Foundation, Inc.
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
--  This package defines the C import to access to the BFD C library.
--
with Interfaces.C.Strings;
package Bfd.Thin.Disassembler is

   pragma Linker_Options ("-lopcodes");

   function Disassembler_Init (Data    : in Ptr;
                               Bfd     : in Ptr;
                               Options : in Interfaces.C.Strings.chars_ptr) return Ptr;
   pragma Import (C, Disassembler_Init, "bfd_ada_disassembler_init");

   function Disassemble (Bfd  : in Ptr;
                         Data : in Ptr;
                         Addr : in Vma_Type)
                         return Integer;
   pragma Import (C, Disassemble, "bfd_ada_disassembler_disassemble");

   procedure Set_Buffer (D : Ptr; Buf : Ptr; Len : Integer; Addr : Vma_Type);
   pragma Import (C, Set_Buffer, "bfd_ada_disassembler_set_buffer");

   procedure Set_Symbol_Table (D     : in Ptr;
                               Syms  : in Ptr;
                               Count : in Integer);
   pragma Import (C, Set_Symbol_Table, "bfd_ada_disassembler_set_symbol_table");

end Bfd.Thin.Disassembler;
