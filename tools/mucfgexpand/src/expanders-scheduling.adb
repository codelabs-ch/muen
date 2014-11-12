--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;

package body Expanders.Scheduling
is

   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Add_Barrier_Configs (Data : in out Muxml.XML_Data_Type)
   is
      Major_Frames : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Major_Frames) - 1 loop
         declare
            Major_Frame      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Major_Frames,
                 Index => I);
            Barriers_Node    : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => "barriers");
            Minor_Exit_Times : constant Mutools.XML_Utils.Deadline_Array
              := Mutools.XML_Utils.Get_Minor_Frame_Deadlines
                (Major => Major_Frame);
            Major_End_Ticks  : constant Interfaces.Unsigned_64
              := Minor_Exit_Times (Minor_Exit_Times'Last).Exit_Time;

            Cur_Barrier_Idx  : Positive := 1;
            Cur_Barrier_Size : Positive := 1;
            Prev_Deadline    : Mutools.XML_Utils.Deadline_Type
              := (Exit_Time   => 0,
                  Minor_Frame => null);
         begin
            for I in Minor_Exit_Times'Range loop
               declare
                  Cur_Deadline : constant Mutools.XML_Utils.Deadline_Type
                    := Minor_Exit_Times (I);
                  Barrier_ID   : constant String
                    := Ada.Strings.Fixed.Trim
                      (Source => Cur_Barrier_Idx'Img,
                       Side   => Ada.Strings.Left);
               begin
                  if Cur_Deadline.Exit_Time = Prev_Deadline.Exit_Time
                    and then Cur_Deadline.Exit_Time /= Major_End_Ticks
                  then
                     Cur_Barrier_Size := Cur_Barrier_Size + 1;
                     if Cur_Barrier_Size = 2 then
                        DOM.Core.Elements.Set_Attribute
                          (Elem  => Prev_Deadline.Minor_Frame,
                           Name  => "barrier",
                           Value => Barrier_ID);
                     end if;
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Cur_Deadline.Minor_Frame,
                        Name  => "barrier",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Cur_Barrier_Idx'Img,
                           Side   => Ada.Strings.Left));
                  else
                     if Cur_Barrier_Size > 1 then
                        declare
                           Size_Str     : constant String
                             := Ada.Strings.Fixed.Trim
                               (Source => Cur_Barrier_Size'Img,
                                Side   => Ada.Strings.Left);
                           Barrier_Node : constant DOM.Core.Node
                             := DOM.Core.Documents.Create_Element
                               (Doc      => Data.Doc,
                                Tag_Name => "barrier");
                        begin
                           Mulog.Log
                             (Msg => "Adding barrier with ID " & Barrier_ID
                              & " to major frame" & I'Img & ": size "
                              & Size_Str & ", ticks"
                              & Prev_Deadline.Exit_Time'Img);
                           DOM.Core.Elements.Set_Attribute
                             (Elem  => Barrier_Node,
                              Name  => "id",
                              Value => Barrier_ID);
                           DOM.Core.Elements.Set_Attribute
                             (Elem  => Barrier_Node,
                              Name  => "size",
                              Value => Size_Str);
                           Muxml.Utils.Append_Child
                             (Node      => Barriers_Node,
                              New_Child => Barrier_Node);
                        end;
                        Cur_Barrier_Size := 1;
                        Cur_Barrier_Idx  := Cur_Barrier_Idx + 1;
                     end if;
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Cur_Deadline.Minor_Frame,
                        Name  => "barrier",
                        Value => "none");
                  end if;

                  Prev_Deadline := Cur_Deadline;
               end;
            end loop;

            Muxml.Utils.Append_Child
              (Node      => Major_Frame,
               New_Child => Barriers_Node);
         end;
      end loop;
   end Add_Barrier_Configs;

end Expanders.Scheduling;
