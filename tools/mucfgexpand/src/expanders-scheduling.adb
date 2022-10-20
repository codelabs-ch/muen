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

            --  Returns True if the given minor frame deadline is the end of
            --  the major frame.
            function Is_Major_Frame_End
              (Deadline : Mutools.XML_Utils.Deadline_Type)
               return Boolean;

            ----------------------------------------------------------------

            function Is_Major_Frame_End
              (Deadline : Mutools.XML_Utils.Deadline_Type)
               return Boolean
            is
            begin
               return Deadline.Exit_Time = Major_End_Ticks;
            end Is_Major_Frame_End;
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
                    and then not Is_Major_Frame_End (Deadline => Cur_Deadline)
                  then

                     --  Current and previous minor frame have same deadline.

                     Cur_Barrier_Size := Cur_Barrier_Size + 1;

                     if Cur_Barrier_Size = 2 then

                        --  Set barrer ID of first matching minor frame.

                        DOM.Core.Elements.Set_Attribute
                          (Elem  => Prev_Deadline.Minor_Frame,
                           Name  => "barrier",
                           Value => Barrier_ID);
                     end if;
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Cur_Deadline.Minor_Frame,
                        Name  => "barrier",
                        Value => Barrier_ID);
                  else
                     if Cur_Barrier_Size > 1 then

                        --  New minor frame does not synchronize on current
                        --  barrier. Add barrier and start fresh with next one.

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

   -------------------------------------------------------------------------

   procedure Add_Group_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Group_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/partitions/partition/group");
      Group_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Group_Nodes);
   begin
      Mulog.Log (Msg => "Setting ID of" & Group_Count'Img
                 & " scheduling group(s)");

      for I in 0 .. Group_Count - 1 loop
         declare
            Group_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Group_Nodes,
                 Index => I);
            ID_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => Natural'Image (I + 1),
               Side   => Ada.Strings.Left);
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Group_Node,
               Name  => "id",
               Value => ID_Str);
         end;
      end loop;
   end Add_Group_IDs;

   -------------------------------------------------------------------------

   procedure Add_Partition_CPU_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Minor_Frames : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame");
      Partition_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/partitions/partition");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Partition_Nodes) - 1 loop
         declare
            Partition_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Partition_Nodes,
                 Index => I);
            Partition_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Partition_Node,
                 Name => "name");
            CPU : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node
                (N => Muxml.Utils.Get_Element
                   (Nodes     => Minor_Frames,
                    Ref_Attr  => "partition",
                    Ref_Value => Partition_Name));
            CPU_ID_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
         begin
            Mulog.Log (Msg => "Setting CPU of scheduling partition '"
                       & Partition_Name & "' to " & CPU_ID_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Partition_Node,
               Name  => "cpu",
               Value => CPU_ID_Str);
         end;
      end loop;
   end Add_Partition_CPU_IDs;

   -------------------------------------------------------------------------

   procedure Add_Partition_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Partition_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/partitions/partition");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Partition_Nodes) - 1 loop
         declare
            Partition_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Partition_Nodes,
                 Index => I);
            Partition_Name : constant String
              := DOM.Core.Elements.Get_Attribute
              (Elem => Partition_Node,
               Name => "name");
            ID_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => Natural'Image (I + 1),
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting ID of scheduling partition '"
                       & Partition_Name & "' to " & ID_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Partition_Node,
               Name  => "id",
               Value => ID_Str);
         end;
      end loop;
   end Add_Partition_IDs;

end Expanders.Scheduling;
