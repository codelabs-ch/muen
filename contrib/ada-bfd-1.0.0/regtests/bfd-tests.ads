-----------------------------------------------------------------------
--  BFD Tests -- Tests for Binary File Descriptor Library (Ada Interface)
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
with Ada.Strings.Unbounded;
with Util.Tests;

with Bfd;
with Bfd.Files;
package Bfd.Tests is

   type Test_Case;

   type Test_Method_Access is access procedure (T : in out Test_Case);

   type File_Type_Access is access Bfd.Files.File_Type;

   type Test_Case is new Util.Tests.Test_Case with record
      Test_Name : Ada.Strings.Unbounded.Unbounded_String;
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      File      : File_Type_Access;
      Method    : Test_Method_Access;
   end record;
   type Test_Case_Access is access all Test_Case'Class;

   --  Provide name identifying the test case:
   overriding
   function Name (T : in Test_Case) return Util.Tests.Message_String;

   --  Setup before each routine:
   overriding
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   overriding
   procedure Tear_Down (T :  in out Test_Case);

   --  Perform the test.
   overriding
   procedure Run_Test (T : in out Test_Case);

   --  Test the Open, Close and Is_Open operations.
   procedure Test_Open (T : in out Test_Case);

   --  Test the Get_Flags operations.
--     procedure Test_Get_Flags (T : in out Test_Case);
   --  Test the Get_File_Flags operation on a binary executable.
   procedure Test_Get_Binary_Flags (T    : in out Test_Case);

   --  Test the Get_File_Flags operation on a object file.
   procedure Test_Get_Object_Flags (T    : in out Test_Case);

   --  Test the Get_File_Flags operation on a object file.
   procedure Test_Get_Debug_Flags (T    : in out Test_Case);

   procedure Test_Basic (T : in out Test_Case);

   function Get_Test_File (T : in Test_Case) return String;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

end Bfd.Tests;
