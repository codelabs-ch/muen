-----------------------------------------------------------------------
--  print -- Print Utilities for examples
--  Copyright (C) 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
with Bfd;
with Bfd.Disassembler;
package Utils is

   procedure Print (Item : in String; Max_Len : in Integer);
   --  Print

   function HexImage (Addr : in Bfd.Vma_Type) return String;

   type Small_Disassembler is new Bfd.Disassembler.Memory_Disassembler_Info_Type
     with null record;

   procedure Output (Info : in out Small_Disassembler;
                     Item : in String);

end Utils;
