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

with Unicode.Encodings;         use Unicode.Encodings;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.CCS;               use Unicode.CCS;

package body Input_Sources is

   -----------------
   -- Prolog_Size --
   -----------------

   function Prolog_Size (From : Input_Source) return Natural is
   begin
      return From.Prolog_Size;
   end Prolog_Size;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Input    : in out Input_Source;
      Es       : Unicode.CES.Encoding_Scheme) is
   begin
      --  Do not change the encoding if the previous one was detected from the
      --  BOM. For instance, if we have detected UTF16-BE, we do not want an
      --  "encoding='UTF-16'" declaration in the XML document to switch us to
      --  UTF16-LE
      if Input.Prolog_Size = 0                --  no BOM
        or else Input.Es.Length /= Es.Length  --  no the same encoding scheme
      then
         Input.Es := Es;
      end if;
   end Set_Encoding;

   ------------------
   -- Get_Encoding --
   ------------------

   function Get_Encoding (Input : Input_Source)
      return Unicode.CES.Encoding_Scheme is
   begin
      return Input.Es;
   end Get_Encoding;

   -----------------------
   -- Set_Character_Set --
   -----------------------

   procedure Set_Character_Set
     (Input : in out Input_Source;
      Cs    : Unicode.CCS.Character_Set) is
   begin
      Input.Cs := Cs;
   end Set_Character_Set;

   -----------------------
   -- Get_Character_Set --
   -----------------------

   function Get_Character_Set (Input : Input_Source)
      return Unicode.CCS.Character_Set is
   begin
      return Input.Cs;
   end Get_Character_Set;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Input : in out Input_Source; Id : Byte_Sequence) is
   begin
      Free (Input.System_Id);
      Input.System_Id := new Byte_Sequence'(Id);
   end Set_System_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Input : Input_Source) return Byte_Sequence is
   begin
      if Input.System_Id = null then
         return "";
      else
         return Input.System_Id.all;
      end if;
   end Get_System_Id;

   -------------------
   -- Set_Public_Id --
   -------------------

   procedure Set_Public_Id (Input : in out Input_Source; Id : Byte_Sequence) is
   begin
      Free (Input.Public_Id);
      Input.Public_Id := new Byte_Sequence'(Id);
   end Set_Public_Id;

   -------------------
   -- Get_Public_Id --
   -------------------

   function Get_Public_Id (Input : Input_Source) return Byte_Sequence is
   begin
      if Input.Public_Id = null then
         return "";
      else
         return Input.Public_Id.all;
      end if;
   end Get_Public_Id;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Input_Source) is
   begin
      Free (Input.Public_Id);
      Free (Input.System_Id);
   end Close;

   -------------------------
   -- Set_Stream_Encoding --
   -------------------------

   procedure Set_Stream_Encoding
     (Input    : in out Input_Sources.Input_Source'Class;
      Encoding : String)
   is
      Encode : constant Unicode_Encoding := Get_By_Name (Encoding);
   begin
      Set_Encoding      (Input, Encode.Encoding_Scheme);
      Set_Character_Set (Input, Encode.Character_Set);
   end Set_Stream_Encoding;

end Input_Sources;
