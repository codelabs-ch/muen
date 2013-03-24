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

with Sax.Encodings; use Sax.Encodings;
with Unicode.CES;   use Unicode.CES;

package body DOM.Core.Character_Datas is

   ------------
   -- Length --
   ------------

   function Length (N : Character_Data) return Natural is
   begin
      return Encoding.Length (Data (N));
   end Length;

   --------------------
   -- Substring_Data --
   --------------------

   function Substring_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural) return DOM_String
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last :=  Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      return Str (Offs .. Last);
   end Substring_Data;

   -----------------
   -- Append_Data --
   -----------------

   procedure Append_Data (N : Character_Data; Arg : DOM_String) is
   begin
      Set_Data (N, Data (N) & Arg);
   end Append_Data;

   -----------------
   -- Insert_Data --
   -----------------

   procedure Insert_Data
     (N : Character_Data;
      Offset : Natural;
      Arg : DOM_String)
   is
      Str : constant DOM_String := Data (N);
      Pos : constant Integer := Index_From_Offset (Str, Offset, Encoding);
   begin
      if Pos < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Pos - 1) & Arg & Str (Pos .. Str'Last));
   end Insert_Data;

   -----------------
   -- Delete_Data --
   -----------------

   procedure Delete_Data
     (N : Character_Data; Offset : Natural; Count : Natural)
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last := Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Offs - 1) & Str (Last .. Str'Last));
   end Delete_Data;

   ------------------
   -- Replace_Data --
   ------------------

   procedure Replace_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural;
      Arg : DOM_String)
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last := Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Offs - 1) & Arg & Str (Last .. Str'Last));
   end Replace_Data;

end DOM.Core.Character_Datas;
