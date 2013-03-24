------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2008, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Reporter;
with AUnit.Test_Results;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

--  XML reporter (fix AUnit issues and generate in a separate file instead of stdout).
package Util.Tests.Reporter is

   type XML_Reporter is new AUnit.Reporter.Reporter with record
      File : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Report (Engine : XML_Reporter;
                     R      : in out AUnit.Test_Results.Result'Class);

   procedure Report (Engine : XML_Reporter;
                     File   : in out Ada.Text_IO.File_Type;
                     R      : in out AUnit.Test_Results.Result'Class);

end Util.Tests.Reporter;
