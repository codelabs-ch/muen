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

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Types;
with Mutools.XML_Utils;

package body Mucfgcheck.Subject
is

   use Ada.Strings.Unbounded;
   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure CPU_ID (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => XML_Data.Doc,
              XPath => "/system/platform/processor",
              Name  => "logicalCpus"));
      Last_Id   : constant Natural := CPU_Count - 1;
      Nodes     : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "subject",
                       Attr      => "cpu",
                       Name_Attr => "name",
                       Test      => Less_Or_Equal'Access,
                       Right     => Interfaces.Unsigned_64 (Last_Id),
                       Error_Msg => "not in valid range 0 .." & Last_Id'Img);
   end CPU_ID;

   -------------------------------------------------------------------------

   procedure Memory_Types (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Memory : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Nodes       : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']/memory/memory");
   begin
      Mulog.Log (Msg => "Checking memory types of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " subject memory mapping(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Virt_Mem     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subject_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Muxml.Utils.Ancestor_Node (Node  => Virt_Mem,
                                                  Level => 2),
               Name => "name");
            Virt_Name    : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Virt_Mem,
               Name => "logical");
            Phys_Name    : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Virt_Mem,
               Name => "physical");
            Phys_Mem     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_Memory,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
            Mem_Type_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Phys_Mem,
               Name => "type");
            Memory_Type  : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value (Mem_Type_Str);
         begin
            if Memory_Type not in Mutools.Types.Subject_Memory then
               raise Validation_Error with "Logical memory region '"
                 & Virt_Name & "' of subject '" & Subject_Name & "' mapping "
                 & "physical region '" & Phys_Name & "' has invalid type "
                 & Mem_Type_Str;
            end if;
         end;
      end loop;
   end Memory_Types;

   -------------------------------------------------------------------------

   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");

      --  Check that names of Left and Right differ.
      procedure Check_Name_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Name_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Id   : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "id");
         Left_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Id   : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "id");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Subjects with id " & Left_Id & " and "
              & Right_Id & " have identical name '" & Left_Name & "'";
         end if;
      end Check_Name_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Subjects)'Img & " subject name(s)");

      Compare_All (Nodes      => Subjects,
                   Comparator => Check_Name_Inequality'Access);
   end Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure No_IOMMU_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/"
           & "device[starts-with(@physical,'iommu')]");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
      Names : Unbounded_String;
   begin
      if Count > 0 then
         for I in 0 .. Count - 1 loop
            declare
               Subj      : constant DOM.Core.Node
                 := Muxml.Utils.Ancestor_Node
                   (Node  => DOM.Core.Nodes.Item
                      (List  => Nodes,
                       Index => I),
                    Level => 2);
               Subj_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Subj,
                    Name => "name");
            begin
               Names := Names & " '" & Subj_Name & "'";

               if I < Count - 1 then
                  Names := Names & ",";
               end if;
            end;
         end loop;

         raise Validation_Error with "IOMMU device referenced by subject"
           & (if Count > 1 then "s" else "") & To_String (Source => Names);
      end if;
   end No_IOMMU_Device_References;

   -------------------------------------------------------------------------

   procedure Runnability (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
      Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Frames     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame");

      package Subject_CPU_Package is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Natural,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => Ada.Strings.Unbounded."=");

      --  Cache of subject to CPU mapping.
      Subj_CPU_Map : Subject_CPU_Package.Map;

      --  Returns the ID of the CPU that can execute the given subject. If no
      --  CPU can execute the given subject -1 is returned.
      function Get_Executing_CPU (Subject : DOM.Core.Node) return Integer;

      ----------------------------------------------------------------------

      function Get_Executing_CPU (Subject : DOM.Core.Node) return Integer
      is
         use type Subject_CPU_Package.Cursor;

         Subj_Name : constant Unbounded_String
           := To_Unbounded_String
             (DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name"));
         Pos       : constant Subject_CPU_Package.Cursor
           := Subject_CPU_Package.Find
           (Container => Subj_CPU_Map,
            Key       => Subj_Name);
         CPU       : Integer := -1;
      begin
         if Pos = Subject_CPU_Package.No_Element then
            declare
               use type DOM.Core.Node;

               Minor_Frame : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Frames,
                    Ref_Attr  => "subject",
                    Ref_Value => To_String (Subj_Name));
            begin
               if Minor_Frame = null then

                  --  Recursively check if subject is switch target of
                  --  a scheduled subject.

                  declare
                     Src_Subjs    : constant DOM.Core.Node_List
                       := Mutools.XML_Utils.Get_Switch_Sources
                         (Data   => XML_Data,
                          Target => Subject);
                     Switch_Count : constant Integer
                       := DOM.Core.Nodes.Length (List => Src_Subjs);
                  begin
                     for J in 0 .. Switch_Count - 1 loop
                        CPU := Get_Executing_CPU
                          (Subject => DOM.Core.Nodes.Item
                             (List  => Src_Subjs,
                              Index => J));
                        exit when CPU /= -1;
                     end loop;

                     if CPU /= -1 then
                        Subj_CPU_Map.Insert
                          (Key      => Subj_Name,
                           New_Item => CPU);
                     end if;
                  end;
               else
                  CPU := Integer'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => DOM.Core.Nodes.Parent_Node
                            (N => Minor_Frame),
                        Name => "id"));
                  Subj_CPU_Map.Insert
                    (Key      => Subj_Name,
                     New_Item => CPU);
               end if;
            end;
         else
            CPU := Subject_CPU_Package.Element (Position => Pos);
         end if;

         return CPU;
      end Get_Executing_CPU;
   begin
      for I in 0 .. Subj_Count - 1 loop
         declare
            Subject   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
         begin
            Mulog.Log (Msg => "Checking runnability of subject '" & Subj_Name
                       & "'");

            if Get_Executing_CPU (Subject => Subject) = -1 then
               raise Validation_Error with "Subject '" & Subj_Name & "' is "
                 & "neither referenced in the scheduling plan nor "
                 & "schedulable via switch events";
            end if;
         end;
      end loop;
   end Runnability;

end Mucfgcheck.Subject;
