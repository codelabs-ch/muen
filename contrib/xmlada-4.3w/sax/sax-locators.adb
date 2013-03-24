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

with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Sax.Symbols;  use Sax.Symbols;

package body Sax.Locators is
   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (Loc : Locator) return Natural is
   begin
      return Loc.Line;
   end Get_Line_Number;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (Loc : Locator) return Natural is
   begin
      return Loc.Column;
   end Get_Column_Number;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Loc : Locator) return Symbol is
   begin
      return Loc.System_Id;
   end Get_System_Id;

   -------------------
   -- Get_Public_Id --
   -------------------

   function Get_Public_Id (Loc : Locator) return Symbol is
   begin
      return Loc.Public_Id;
   end Get_Public_Id;

   -----------------------
   -- Set_Column_Number --
   -----------------------

   procedure Set_Column_Number (Loc : in out Locator; Column : Natural := 0) is
   begin
      Loc.Column := Column;
   end Set_Column_Number;

   ----------------------------
   -- Increase_Column_Number --
   ----------------------------

   procedure Increase_Column_Number
     (Loc : in out Locator; Inc : Natural := 1) is
   begin
      Loc.Column := Loc.Column + Inc;
   end Increase_Column_Number;

   --------------------------
   -- Increase_Line_Number --
   --------------------------

   procedure Increase_Line_Number (Loc : in out Locator; Inc : Natural := 1) is
   begin
      Loc.Line := Loc.Line + Inc;
   end Increase_Line_Number;

   ---------------------
   -- Set_Line_Number --
   ---------------------

   procedure Set_Line_Number (Loc : in out Locator; Line : Natural := 0) is
   begin
      Loc.Line := Line;
   end Set_Line_Number;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Loc : Locator) return Location is
   begin
      if Loc = null then
         return No_Location;
      else
         return Loc.all;
      end if;
   end Get_Location;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location (Loc : in out Locator; To : Location) is
   begin
      Loc.all := To;
   end Set_Location;

   -------------------
   -- Set_Public_Id --
   -------------------

   procedure Set_Public_Id (Loc : in out Locator; Id : Symbol) is
   begin
      Loc.Public_Id := Id;
   end Set_Public_Id;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Loc : in out Locator; Id : Symbol) is
   begin
      Loc.System_Id := Id;
   end Set_System_Id;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Loc : Locator; Use_Basename : Boolean := False) return String
   is
   begin
      if Loc = null then
         return "";
      else
         return To_String (Loc.all, Use_Basename);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Loc : Location; Use_Basename : Boolean := False) return String
   is
      C    : constant Natural := Loc.Column;
      Line : constant String := Natural'Image (Loc.Line);
      Col  : constant String := Natural'Image (C);
      Public : constant Symbol := Loc.Public_Id;
   begin
      if Public = No_Symbol then
         return "";
      elsif C /= 0 then
         if Use_Basename then
            return (Base_Name (Get (Public).all) & ':'
                    & Line (Line'First + 1 .. Line'Last)
                    & ':' & Col (Col'First + 1 .. Col'Last));
         else
            return (Get (Public).all & ':'
                    & Line (Line'First + 1 .. Line'Last)
                    & ':' & Col (Col'First + 1 .. Col'Last));
         end if;
      else
         if Use_Basename then
            return
              (Base_Name (Get (Public).all)
               & ':' & Line (Line'First + 1 .. Line'Last));
         else
            return (Get (Public).all
                    & ':' & Line (Line'First + 1 .. Line'Last));
         end if;
      end if;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Loc : in out Locator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Location, Locator);
   begin
      Unchecked_Free (Loc);
   end Free;

   ------------
   -- Create --
   ------------

   function Create return Locator is
   begin
      return new Location;
   end Create;

end Sax.Locators;
