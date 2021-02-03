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

   --  Extract name designated by tag from given data. If no name is present
   --  and empty string is returned.
   function Extract_Name
     (Data : String;
      Tag  : String)
      return String;

   -------------------------------------------------------------------------

   function Extract_Name
     (Data : String;
      Tag  : String)
      return String
   is
      Tag_Idx   : constant Natural := Ada.Strings.Fixed.Index
        (Source  => Data,
         Pattern => Tag);
      Start_Idx : constant Natural := Tag_Idx + Tag'Length;
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
         return "";
      end if;

      return Data (Left_Idx .. Right_Idx - 1);
   end Extract_Name;

   -------------------------------------------------------------------------

   procedure Parse_Edge
     (Data   :     String;
      Valid  : out Boolean;
      Source : out Ada.Strings.Unbounded.Unbounded_String;
      Target : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Source_Tag : constant String := "sourcename: """;
      Target_Tag : constant String := "targetname: """;
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

      Extract_Source_Name :
      declare
         Src_Name : constant String
           := Extract_Name (Data => Data,
                            Tag  => Source_Tag);
      begin
         if Src_Name = "" then
            return;
         end if;

         Source := To_Unbounded_String (Src_Name);
      end Extract_Source_Name;

      Extract_Target_Name :
      declare
         Trgt_Name : constant String
           := Extract_Name (Data => Data,
                            Tag  => Target_Tag);
      begin
         if Trgt_Name = "" then
            return;
         end if;

         Target := To_Unbounded_String (Trgt_Name);
      end Extract_Target_Name;

      Valid := True;
   end Parse_Edge;

   -------------------------------------------------------------------------

   procedure Parse_Line
     (Data  :        String;
      Graph : in out Types.Control_Flow_Graph_Type)
   is
   begin
      if Data'Length <= Node_Preamble'Length then
         return;
      end if;

      declare
         Preamble : constant String
           := Data (Data'First .. Data'First + Node_Preamble'Length - 1);
      begin
         if Preamble = Edge_Preamble then
            declare
               Source, Target : Unbounded_String;
               Success        : Boolean;
            begin
               Parse_Edge (Data   => Data,
                           Valid  => Success,
                           Source => Source,
                           Target => Target);
               if Success then
                  Types.Add_Call (Graph       => Graph,
                                  Source_Name => To_String (Source),
                                  Target_Name => To_String (Target));
               end if;
            end;
         elsif Preamble = Node_Preamble then
            declare
               Sub     : Types.Subprogram_Type;
               Success : Boolean;
            begin
               Parse_Node (Data       => Data,
                           Valid      => Success,
                           Subprogram => Sub);
               if Success then
                  Types.Add_Node (Graph      => Graph,
                                  Subprogram => Sub);
               end if;
            end;
         end if;
      end;
   end Parse_Line;

   -------------------------------------------------------------------------

   procedure Parse_Node
     (Data       :     String;
      Valid      : out Boolean;
      Subprogram : out Types.Subprogram_Type)
   is
      Marker        : constant String := " bytes (";
      Title_Tag     : constant String := "title: """;
      Cur_Idx       : Natural;
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Usage         : Natural;
      Dynamic_Stack : Boolean;
      Bounded_Stack : Boolean;
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
         if Left_Idx = 0 or else Left_Idx + 1 > Cur_Idx - 1 then
            return;
         end if;

         Usage := Natural'Value (Data (Left_Idx + 1 .. Cur_Idx - 1));
      end Extract_Stack_Usage;

      Extract_Title :
      declare
         Title : constant String
           := Extract_Name (Data => Data,
                            Tag  => Title_Tag);
      begin
         if Title = "" then
            return;
         end if;

         Name := To_Unbounded_String (Title);
      end Extract_Title;

      Extract_Dynamic :
      declare
         Dynamic_Tag : constant String := "(dynamic";

         Idx : constant Natural
           := Ada.Strings.Fixed.Index
             (Source  => Data,
              Pattern => Dynamic_Tag,
              From    => Cur_Idx + Marker'Length - 1);
      begin
         Dynamic_Stack := Idx /= 0;
      end Extract_Dynamic;

      Extract_Bounded :
      declare
         Bounded_Tag : constant String := "bounded)";

         Idx : constant Natural
           := Ada.Strings.Fixed.Index
             (Source  => Data,
              Pattern => Bounded_Tag,
              From    => Cur_Idx + Marker'Length);
      begin
         Bounded_Stack := Idx /= 0;
      end Extract_Bounded;

      Subprogram := Types.Create (Name          => To_String (Name),
                                  Stack_Usage   => Usage,
                                  Dynamic_Stack => Dynamic_Stack,
                                  Bounded_Stack => Bounded_Stack);

      Valid := True;
   end Parse_Node;

end Stackcheck.Input;
