------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

package body String_Stream is

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out String_Stream_Type;
      Item   : Stream_Element_Array)
   is
      Str : String (1 .. Integer (Item'Length));
      S   : Integer := Str'First;
   begin
      for J in Item'Range loop
         Str (S) := Character'Val (Item (J));
         S := S + 1;
      end loop;
      Append (Stream.Str, Str);
   end Write;

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream : in out String_Stream_Type'Class;
      Str    : String) is
   begin
      Stream.Str        := To_Unbounded_String (Str);
      Stream.Read_Index := 1;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Str : constant String := Slice
        (Stream.Str, Stream.Read_Index, Stream.Read_Index + Item'Length - 1);
      J   : Stream_Element_Offset := Item'First;
   begin
      for S in Str'Range loop
         Item (J) := Stream_Element (Character'Pos (Str (S)));
         J := J + 1;
      end loop;
      Last := Item'First + Str'Length - 1;
      Stream.Read_Index := Stream.Read_Index + Item'Length;
   end Read;

end String_Stream;
