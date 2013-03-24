-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  <!-- Copyright (C) 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with System;
with Interfaces.C.Strings;
package Bfd.Internal is

   subtype Ptr is System.Address;
   subtype Pointer is System.Address;

   type String_Ptr is access String;

   Current_Program_Name : String_Ptr := null;

   function Strlen (Name : in Ptr) return Natural;
   pragma Import (C, Strlen, "strlen");

   function Strcmp (S1, S2 : in Interfaces.C.Strings.chars_ptr) return Integer;
   pragma Import (C, Strcmp, "strcmp");

   procedure Strcpy (To : in Ptr; From : in Ptr);
   pragma Import (C, Strcpy, "strcpy");

   procedure Memcpy (To : in Ptr; From : in Ptr; Size : in Natural);
   pragma Import (C, Memcpy, "memcpy");

end Bfd.Internal;
