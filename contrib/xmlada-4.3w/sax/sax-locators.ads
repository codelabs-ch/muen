------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the Locator type, which is used to identify locations
--  within the XML streams where events have occurred.
--  This type is reference-counted in XML/Ada, which means that memory is
--  managed automatically. However, if you keep a copy of a locator, its
--  attributes will be changed as the XML stream is parsed. You must use Copy
--  to preserve the value of these attributes over time.

with Sax.Symbols;

package Sax.Locators is

   type Locator is private;
   No_Locator : constant Locator;

   function Create return Locator;
   --  Create a new locator

   type Location is record
      Line      : Natural := 1;
      Column    : Natural := 1;
      Public_Id : Sax.Symbols.Symbol := Sax.Symbols.Empty_String;
      System_Id : Sax.Symbols.Symbol := Sax.Symbols.Empty_String;
   end record;
   No_Location : constant Location :=
     (1, 1, Sax.Symbols.Empty_String, Sax.Symbols.Empty_String);
   --  A static location, ie a location that will not be changed automatically
   --  by the parser (as opposed to a [Locator] which is basically a pointer to
   --  such a location, modified dynamically by the parser).
   --  For efficiency, a [Location] is made a public record, where you can
   --  access the fields directly.

   procedure Set_Line_Number (Loc : in out Locator; Line : Natural := 0);
   function Get_Line_Number (Loc : Locator) return Natural;
   pragma Inline (Get_Line_Number, Set_Line_Number);
   --  Return the line number where the current document event ends

   procedure Set_Column_Number (Loc : in out Locator; Column : Natural := 0);
   function Get_Column_Number  (Loc : Locator) return Natural;
   pragma Inline (Get_Column_Number, Set_Column_Number);
   --  Return the column number where the current document event ends

   procedure Increase_Line_Number (Loc : in out Locator; Inc : Natural := 1);
   procedure Increase_Column_Number (Loc : in out Locator; Inc : Natural := 1);
   pragma Inline (Increase_Column_Number, Increase_Line_Number);
   --  Increment the column number. This assume Loc has already been
   --  initialized

   procedure Set_System_Id (Loc : in out Locator; Id : Sax.Symbols.Symbol);
   function Get_System_Id (Loc : Locator) return Sax.Symbols.Symbol;
   pragma Inline (Set_System_Id, Get_System_Id);
   --  Return the system id for the current document (see input_sources.ads)

   procedure Set_Public_Id (Loc : in out Locator; Id : Sax.Symbols.Symbol);
   function Get_Public_Id (Loc : Locator) return Sax.Symbols.Symbol;
   pragma Inline (Set_Public_Id, Get_Public_Id);
   --  Return the public id for the current document (see input_sources.ads)

   function To_String
     (Loc : Location; Use_Basename : Boolean := False) return String;
   function To_String
     (Loc : Locator; Use_Basename : Boolean := False) return String;
   --  Print the location found in the location, with a standard format:
   --     Public_Id:Line:Column
   --  Public_Id is not printed if it is null.
   --  Column is not printed if it is zero (unknown)
   --  If Use_Basename is true, then the file name will not include any
   --  directory specification.

   procedure Set_Location (Loc : in out Locator; To : Location);
   function Get_Location (Loc : Locator) return Location;
   pragma Inline (Set_Location, Get_Location);
   --  Get the current location information.

   procedure Free (Loc : in out Locator);
   --  Free the memory occupied by Loc

private
   type Locator is access Location;
   No_Locator : constant Locator := null;
end Sax.Locators;
