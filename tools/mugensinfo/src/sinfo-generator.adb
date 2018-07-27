--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.PCI;
with Mutools.Utils;
with Mutools.Match;

with Musinfo;

with Sinfo.Utils;
with Sinfo.Writer;
with Sinfo.Constants;

package body Sinfo.Generator
is

   --  Add memory region with given physical and virtual memory nodes of
   --  specified subject to given subject info data.
   procedure Add_Memregion_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node);

   --  Add device data to given subject info.
   procedure Add_Device_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Logical_Dev   :        DOM.Core.Node;
      Physical_Dev  :        DOM.Core.Node;
      Physical_Name :        String);

   --  Add event to given subject info.
   procedure Add_Event_To_Info
     (Info         : in out Musinfo.Subject_Info_Type;
      Subject_Name :        String;
      Logical_Evt  :        DOM.Core.Node);

   --  Add vector to given subject info.
   procedure Add_Vector_To_Info
     (Info         : in out Musinfo.Subject_Info_Type;
      Subject_Name :        String;
      Logical_Vec  :        DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Add_Device_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Logical_Dev   :        DOM.Core.Node;
      Physical_Dev  :        DOM.Core.Node;
      Physical_Name :        String)
   is
      use type Interfaces.Unsigned_64;
      use type DOM.Core.Node;

      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Logical_Dev,
           Name => "logical");
      SID : constant Interfaces.Unsigned_16 := Mutools.PCI.To_SID
        (BDF => Mutools.PCI.Get_BDF
           (Dev => Logical_Dev));
      IRQ_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Logical_Dev,
           XPath => "irq");
      MSI : Boolean := False;
      IRQ_Start, IRQ_End, IRTE_Start, IRTE_End,
      IR_Count : Interfaces.Unsigned_64 := 0;
   begin
      if IRQ_Node /= null then
         declare
            Vecs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Logical_Dev,
                 XPath => "irq");
            IRQs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Physical_Dev,
                 XPath => "irq");
            Linked : constant Muxml.Utils.Matching_Pairs_Type
              := Muxml.Utils.Get_Matching
                (Left_Nodes     => Vecs,
                 Right_Nodes    => IRQs,
                 Match_Multiple => True,
                 Match          => Mutools.Match.Is_Valid_Reference'Access);
         begin
            MSI := Muxml.Utils.Get_Attribute
              (Doc   => Physical_Dev,
               XPath => "pci",
               Name  => "msi") = "true";

            Muxml.Utils.Get_Bounds
              (Nodes     => Vecs,
               Attr_Name => "vector",
               Lower     => IRQ_Start,
               Upper     => IRQ_End);
            Muxml.Utils.Get_Bounds
              (Nodes     => Linked.Right,
               Attr_Name => "number",
               Lower     => IRTE_Start,
               Upper     => IRTE_End);
            IR_Count := IRQ_End - IRQ_Start + 1;
         end;
      end if;

      Mulog.Log
        (Msg => "Announcing device to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Physical_Name & "], SID "
         & Mutools.Utils.To_Hex (Number => Interfaces.Unsigned_64 (SID))
         & (if IRQ_Node /= null then ", IRTE" & IRTE_Start'Img & " .."
           & IRTE_End'Img & ", IRQ" & IRQ_Start'Img & " .." & IRQ_End'Img
           & (if MSI then ", MSI" else "") else ""));

      Utils.Append_Resource
        (Info     => Info,
         Resource =>
           (Kind     => Musinfo.Res_Device,
            Name     => Utils.Create_Name (Str => Log_Name),
            Dev_Data =>
              (SID        => SID,
               IRTE_Start => Interfaces.Unsigned_16 (IRTE_Start),
               IRQ_Start  => Interfaces.Unsigned_8 (IRQ_Start),
               IR_Count   => Interfaces.Unsigned_8 (IR_Count),
               Flags      => (MSI_Capable => MSI,
                              Padding     => 0),
               Padding    => (others => 0)),
            Padding  => (others => 0)));
   end Add_Device_To_Info;

   -------------------------------------------------------------------------

   procedure Add_Event_To_Info
     (Info         : in out Musinfo.Subject_Info_Type;
      Subject_Name :        String;
      Logical_Evt  :        DOM.Core.Node)
   is
      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Logical_Evt,
           Name => "logical");
      Number : constant Interfaces.Unsigned_8
        := Interfaces.Unsigned_8'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Evt,
              Name => "id"));
   begin
      Mulog.Log
        (Msg => "Announcing event number to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Ada.Strings.Fixed.Trim
           (Source => Number'Img,
            Side   => Ada.Strings.Left) & "]");

      Utils.Append_Resource
        (Info     => Info,
         Resource => (Kind     => Musinfo.Res_Event,
                      Name     => Utils.Create_Name (Str => Log_Name),
                      Evt_Data => (Value   => Number,
                                   Padding => (others => 0)),
                      Padding  => (others => 0)));
   end Add_Event_To_Info;

   -------------------------------------------------------------------------

   procedure Add_Memregion_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node)
   is
      R : Musinfo.Memregion_Type;

      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Virt_Mem_Node,
           Name => "logical");
      Phys_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phys_Mem_Node,
           Name => "name");
   begin
      R := Utils.Get_Memory_Info
        (Virt_Mem_Node => Virt_Mem_Node,
         Phys_Mem_Node => Phys_Mem_Node);

      Mulog.Log
        (Msg => "Announcing memregion to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Phys_Name & "]@"
         & Mutools.Utils.To_Hex (Number => R.Address)
         & ", size " & Mutools.Utils.To_Hex (Number => R.Size) & ", "
         & (if R.Flags.Writable   then "writable" else "read-only") & ", "
         & (if R.Flags.Executable then "executable" else "non-executable")
         & (if R.Flags.Channel then ", channel" else "")
         & ", " & R.Content'Img);

      Utils.Append_Resource
        (Info     => Info,
         Resource => (Kind     => Musinfo.Res_Memory,
                      Name     => Utils.Create_Name (Str => Log_Name),
                      Mem_Data => R,
                      Padding  => (others => 0)));
   end Add_Memregion_To_Info;

   -------------------------------------------------------------------------

   procedure Add_Vector_To_Info
     (Info         : in out Musinfo.Subject_Info_Type;
      Subject_Name :        String;
      Logical_Vec  :        DOM.Core.Node)
   is
      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Logical_Vec,
           Name => "logical");
      Number : constant Interfaces.Unsigned_8
        := Interfaces.Unsigned_8'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Muxml.Utils.Get_Element
                (Doc   => Logical_Vec,
                 XPath => "inject_interrupt"),
              Name => "vector"));
   begin
      Mulog.Log
        (Msg => "Announcing vector number to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Ada.Strings.Fixed.Trim
           (Source => Number'Img,
            Side   => Ada.Strings.Left) & "]");

      Utils.Append_Resource
        (Info     => Info,
         Resource => (Kind     => Musinfo.Res_Vector,
                      Name     => Utils.Create_Name (Str => Log_Name),
                      Vec_Data => (Value   => Number,
                                   Padding => (others => 0)),
                      Padding  => (others => 0)));
   end Add_Vector_To_Info;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      TSC_Khz  : constant Interfaces.Unsigned_64
        := 1000 * Interfaces.Unsigned_64'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/hardware/processor",
              Name  => "speed"));
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Phys_Dev : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Sinfos   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_info']/file");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Sinfos)'Img
                 & " subject info file(s)");
      Mulog.Log (Msg => "Announcing TSC tick rate of" & TSC_Khz'Img & " kHz");

      for I in 0 .. DOM.Core.Nodes.Length (List => Sinfos) - 1 loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Sinfos,
                                      Index => I);
            Filename : constant String
              := Output_Dir & "/" & DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
            Memname  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "name");
            Subj_Name : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Memname);
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subjects,
                 Ref_Attr  => "name",
                 Ref_Value => Subj_Name);
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Subj_Devs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "devices/device[pci]");
            Subj_Evts : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "events/source/group[@name='vmcall']/event");
            Subj_Vecs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "events/target/event[inject_interrupt]");
            Subject_Info : Musinfo.Subject_Info_Type
              := Constants.Null_Subject_Info;
         begin
            Subject_Info.Name := Utils.Create_Name (Str => Subj_Name);
            Subject_Info.TSC_Khz := TSC_Khz;

            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Memory) - 1 loop
               declare
                  Virt_Mem_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subj_Memory,
                       Index => J);
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Virt_Mem_Node,
                       Name => "physical");
                  Phys_Mem_Node : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Mem,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Name);
               begin
                  Add_Memregion_To_Info
                    (Info          => Subject_Info,
                     Subject_Name  => Subj_Name,
                     Virt_Mem_Node => Virt_Mem_Node,
                     Phys_Mem_Node => Phys_Mem_Node);
               end;
            end loop;

            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Devs) - 1 loop
               declare
                  Logical_Device : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Subj_Devs,
                                            Index => J);
                  Physical_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Device,
                       Name => "physical");
                  Physical_Device : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Dev,
                       Ref_Attr  => "name",
                       Ref_Value => Physical_Name);
               begin
                  Add_Device_To_Info
                    (Info          => Subject_Info,
                     Subject_Name  => Subj_Name,
                     Logical_Dev   => Logical_Device,
                     Physical_Dev  => Physical_Device,
                     Physical_Name => Physical_Name);
               end;
            end loop;

            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Evts) - 1 loop
               Add_Event_To_Info (Info         => Subject_Info,
                                  Subject_Name => Subj_Name,
                                  Logical_Evt  => DOM.Core.Nodes.Item
                                    (List  => Subj_Evts,
                                     Index => J));
            end loop;

            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Vecs) - 1 loop
               Add_Vector_To_Info (Info         => Subject_Info,
                                   Subject_Name => Subj_Name,
                                   Logical_Vec  => DOM.Core.Nodes.Item
                                     (List  => Subj_Vecs,
                                      Index => J));
            end loop;

            Mulog.Log (Msg => "Writing subject info data to '" & Filename
                       & "'");
            Writer.Serialize
              (Info     => Subject_Info,
               Filename => Filename);
         end;
      end loop;
   end Write;

end Sinfo.Generator;
