-----------------------------------------------------------------------
--  BFD Tests -- Tests for BFD section Ada API
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
with Util.Tests;
with Bfd;
with Bfd.Tests;
package Bfd.Sections.Tests is

   subtype Test_Case is Bfd.Tests.Test_Case;

   --  Test get section content operations
   procedure Test_Get_Section_Contents (T : in out Test_Case);

   --  Test find sections operations
   procedure Test_Find_Section (T : in out Test_Case);

   --  Test basic sections operations
   procedure Test_Sections (T : in out Test_Case);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

end Bfd.Sections.Tests;
