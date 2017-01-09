--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body Stackcheck.Input
is

   use Ada.Strings.Unbounded;

   Edge_Preamble : constant String := "edge:";
   Node_Preamble : constant String := "node:";

   -------------------------------------------------------------------------

   procedure Parse_Edge
     (Data   :     String;
      Valid  : out Boolean;
      Source : out Ada.Strings.Unbounded.Unbounded_String;
      Target : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Source_Tag : constant String := "sourcename: """;
      Target_Tag : constant String := "targetname: """;
      Cur_Idx    : Natural;
   begin
      Valid  := False;
      Source := Null_Unbounded_String;
      Target := Null_Unbounded_String;

      if Data'Length <= Edge_Preamble'Length or else
        Data (Data'First .. Data'First + Edge_Preamble'Length - 1)
        /= Edge_Preamble
      then
         return;
      end if;

      Cur_Idx := Ada.Strings.Fixed.Index
        (Source  => Data,
         Pattern => Source_Tag);

      Extract_Source_Name :
      declare
         Start_Idx : constant Natural := Cur_Idx + Source_Tag'Length;
         Right_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => """",
                                       From    => Start_Idx);
         Colon_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => ":",
                                       From    => Right_Idx,
                                       Going   => Ada.Strings.Backward);
         Left_Idx  : constant Natural
           := Natural'Max (Start_Idx, Colon_Idx + 1);
      begin
         if Right_Idx <= Left_Idx then
            return;
         end if;

         Source := To_Unbounded_String (Data (Left_Idx .. Right_Idx - 1));
      end Extract_Source_Name;

      Cur_Idx := Ada.Strings.Fixed.Index
        (Source  => Data,
         Pattern => Target_Tag);

      Extract_Target_Name :
      declare
         Start_Idx : constant Natural := Cur_Idx + Target_Tag'Length;
         Right_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => """",
                                       From    => Start_Idx);
         Colon_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => ":",
                                       From    => Right_Idx,
                                       Going   => Ada.Strings.Backward);
         Left_Idx  : constant Natural
           := Natural'Max (Start_Idx, Colon_Idx + 1);
      begin
         if Right_Idx <= Left_Idx then
            return;
         end if;

         Target := To_Unbounded_String (Data (Left_Idx .. Right_Idx - 1));
      end Extract_Target_Name;

      Valid := True;
   end Parse_Edge;

   -------------------------------------------------------------------------

   procedure Parse_Node
     (Data       :     String;
      Valid      : out Boolean;
      Subprogram : out Types.Subprogram_Type)
   is
      Marker  : constant String := " bytes (";
      Title   : constant String := "title: """;
      Cur_Idx : Natural;
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Usage   : Natural;
   begin
      Valid      := False;
      Subprogram := Types.Null_Subprogram;

      if Data'Length <= Node_Preamble'Length or else
        Data (Data'First .. Data'First + Node_Preamble'Length - 1)
        /= Node_Preamble
      then
         return;
      end if;

      --  Look for subprogram marker.

      Cur_Idx := Ada.Strings.Fixed.Index
        (Source  => Data,
         Pattern => Marker);

      if Cur_Idx = 0 then
         return;
      end if;

      Extract_Stack_Usage :
      declare
         Left_Idx : constant Natural
           := Ada.Strings.Fixed.Index
             (Source => Data,
              Set    => Ada.Strings.Maps.Constants.Decimal_Digit_Set,
              From   => Cur_Idx - 1,
              Test   => Ada.Strings.Outside,
              Going  => Ada.Strings.Backward);
      begin
         if Left_Idx = 0 or else Left_Idx + 1 >= Cur_Idx - 1 then
            return;
         end if;

         Usage := Natural'Value (Data (Left_Idx + 1 .. Cur_Idx - 1));
      end Extract_Stack_Usage;

      Cur_Idx := Ada.Strings.Fixed.Index
        (Source  => Data,
         Pattern => Title);

      Extract_Name :
      declare
         Start_Idx : constant Natural := Cur_Idx + Title'Length;
         Right_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => """",
                                       From    => Start_Idx);
         Colon_Idx : constant Natural
           := Ada.Strings.Fixed.Index (Source  => Data,
                                       Pattern => ":",
                                       From    => Right_Idx,
                                       Going   => Ada.Strings.Backward);
         Left_Idx  : constant Natural
           := Natural'Max (Start_Idx, Colon_Idx + 1);
      begin
         if Right_Idx <= Left_Idx then
            return;
         end if;

         Name := To_Unbounded_String (Data (Left_Idx .. Right_Idx - 1));
      end Extract_Name;

      Subprogram := Types.Create (Name        => To_String (Name),
                                  Stack_Usage => Usage);
      Valid := True;
   end Parse_Node;

end Stackcheck.Input;
