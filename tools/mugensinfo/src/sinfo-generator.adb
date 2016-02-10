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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.PCI;
with Mutools.Utils;
with Mutools.Types;
with Musinfo.Utils;
with Musinfo.Writer;

with Sinfo.Utils;

package body Sinfo.Generator
is

   --  Add channel with given physical and virtual memory regions of specified
   --  subject to subject info data.
   procedure Add_Channel_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Node  :        DOM.Core.Node;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node);

   --  Add memory region with given physical and virtual memory nodes of
   --  specified subject to given subject info data.
   procedure Add_Memregion_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node);

   --  Get memory region information from given virtual and physical memory
   --  nodes.
   procedure Get_Memory_Info
     (Virt_Mem_Node :     DOM.Core.Node;
      Phys_Mem_Node :     DOM.Core.Node;
      Address       : out Interfaces.Unsigned_64;
      Size          : out Interfaces.Unsigned_64;
      Writable      : out Boolean;
      Executable    : out Boolean);

   --  Add device data to given subject info.
   procedure Add_Device_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Logical_Dev   :        DOM.Core.Node;
      Physical_Dev  :        DOM.Core.Node;
      Physical_Name :        String);

   -------------------------------------------------------------------------

   procedure Add_Channel_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Node  :        DOM.Core.Node;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node)
   is
      Address, Size        : Interfaces.Unsigned_64;
      Writable, Executable : Boolean;

      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Virt_Mem_Node,
           Name => "logical");
      Phys_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phys_Mem_Node,
           Name => "name");
      Event_ID_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Subject_Node,
           XPath => "events/source/group/event/notify[@physical='"
           & Phys_Name & "']/..",
           Name  => "id");
      Has_Event : constant Boolean := Event_ID_Str'Length > 0;
      Event_Nr  : Musinfo.Event_Number_Range
        := Musinfo.Event_Number_Range'First;
      Vector_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Subject_Node,
           XPath => "events/target/event[@physical='" & Phys_Name & "']",
           Name  => "vector");
      Has_Vector : constant Boolean     := Vector_Str'Length > 0;
      Vector     : Musinfo.Vector_Range := Musinfo.Vector_Range'First;
   begin
      Get_Memory_Info
        (Virt_Mem_Node => Virt_Mem_Node,
         Phys_Mem_Node => Phys_Mem_Node,
         Address       => Address,
         Size          => Size,
         Writable      => Writable,
         Executable    => Executable);

      if Has_Event then
         Event_Nr := Musinfo.Event_Number_Range'Value (Event_ID_Str);
      end if;

      if Has_Vector then
         Vector := Musinfo.Vector_Range'Value (Vector_Str);
      end if;

      Mulog.Log
        (Msg => "Announcing channel to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Phys_Name & "]@"
         & Mutools.Utils.To_Hex (Number => Address)
         & ", size " & Mutools.Utils.To_Hex (Number => Size) & ", "
         & (if Writable   then "writable" else "read-only")
         & (if Has_Event  then ", event "  & Event_ID_Str else "")
         & (if Has_Vector then ", vector " & Vector_Str   else ""));

      Musinfo.Utils.Append_Channel
        (Info       => Info,
         Name       => Musinfo.Utils.Create_Name (Str => Log_Name),
         Address    => Address,
         Size       => Size,
         Writable   => Writable,
         Has_Event  => Has_Event,
         Has_Vector => Has_Vector,
         Event      => Event_Nr,
         Vector     => Vector);
   end Add_Channel_To_Info;

   -------------------------------------------------------------------------

   procedure Add_Device_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Logical_Dev   :        DOM.Core.Node;
      Physical_Dev  :        DOM.Core.Node;
      Physical_Name :        String)
   is
      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Logical_Dev,
           Name => "logical");
      MSI : constant Boolean
        := Muxml.Utils.Get_Attribute
          (Doc   => Physical_Dev,
           XPath => "pci",
           Name  => "msi") = "true";
      Vecs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Logical_Dev,
           XPath => "irq");
      IRQs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Physical_Dev,
           XPath => "irq");
      SID : constant Interfaces.Unsigned_16 := Mutools.PCI.To_SID
        (BDF => Mutools.PCI.Get_BDF
           (Dev => Logical_Dev));
      IRQ_Start, IRQ_End   : Positive;
      IRTE_Start, IRTE_End : Positive;
   begin
      Utils.Get_Bounds (Nodes     => Vecs,
                        Attr_Name => "vector",
                        Lower     => IRQ_Start,
                        Upper     => IRQ_End);
      Utils.Get_Bounds (Nodes     => IRQs,
                        Attr_Name => "number",
                        Lower     => IRTE_Start,
                        Upper     => IRTE_End);
      Mulog.Log
        (Msg => "Announcing device to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Physical_Name & "], SID "
         & Mutools.Utils.To_Hex (Number => Interfaces.Unsigned_64 (SID))
         & ", IRTE" & IRTE_Start'Img & " .." & IRTE_End'Img
         & ", IRQ" & IRQ_Start'Img & " .." & IRQ_End'Img
         & (if MSI then ", MSI" else ""));

      Musinfo.Utils.Append_Dev
        (Info        => Info,
         SID         => SID,
         IRTE_Start  => Interfaces.Unsigned_16 (IRTE_Start),
         IRQ_Start   => Interfaces.Unsigned_8 (IRQ_Start),
         IR_Count    => Interfaces.Unsigned_8 (IRQ_End - IRQ_Start + 1),
         MSI_Capable => MSI);
   end Add_Device_To_Info;

   -------------------------------------------------------------------------

   procedure Add_Memregion_To_Info
     (Info          : in out Musinfo.Subject_Info_Type;
      Subject_Name  :        String;
      Virt_Mem_Node :        DOM.Core.Node;
      Phys_Mem_Node :        DOM.Core.Node)
   is
      Address, Size        : Interfaces.Unsigned_64;
      Writable, Executable : Boolean;

      Log_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Virt_Mem_Node,
           Name => "logical");
      Phys_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phys_Mem_Node,
           Name => "name");
   begin
      Get_Memory_Info
        (Virt_Mem_Node => Virt_Mem_Node,
         Phys_Mem_Node => Phys_Mem_Node,
         Address       => Address,
         Size          => Size,
         Writable      => Writable,
         Executable    => Executable);

      Mulog.Log
        (Msg => "Announcing memregion to subject '" & Subject_Name
         & "': " & Log_Name & "[" & Phys_Name & "]@"
         & Mutools.Utils.To_Hex (Number => Address)
         & ", size " & Mutools.Utils.To_Hex (Number => Size) & ", "
         & (if Writable   then "writable" else "read-only") & ", "
         & (if Executable then "executable" else "non-executable"));

      Musinfo.Utils.Append_Memregion
        (Info       => Info,
         Name       => Musinfo.Utils.Create_Name (Str => Log_Name),
         Address    => Address,
         Size       => Size,
         Writable   => Writable,
         Executable => Executable);
   end Add_Memregion_To_Info;

   -------------------------------------------------------------------------

   procedure Get_Memory_Info
     (Virt_Mem_Node :     DOM.Core.Node;
      Phys_Mem_Node :     DOM.Core.Node;
      Address       : out Interfaces.Unsigned_64;
      Size          : out Interfaces.Unsigned_64;
      Writable      : out Boolean;
      Executable    : out Boolean)
   is
   begin
      Address := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Virt_Mem_Node,
            Name => "virtualAddress"));
      Size := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Phys_Mem_Node,
            Name => "size"));
      Writable := Boolean'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Virt_Mem_Node,
            Name => "writable"));
      Executable := Boolean'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Virt_Mem_Node,
            Name => "executable"));
   end Get_Memory_Info;

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
                 XPath => "devices/device[pci and irq]");
            Subject_Info : Musinfo.Subject_Info_Type
              := Musinfo.Null_Subject_Info;
         begin
            Subject_Info.TSC_Khz := TSC_Khz;

            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Memory) - 1 loop
               declare
                  use type Mutools.Types.Memory_Kind;

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
                  Mem_Type : constant Mutools.Types.Memory_Kind
                    := Mutools.Types.Memory_Kind'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Mem_Node,
                          Name => "type"));
               begin
                  if Mem_Type = Mutools.Types.Subject_Channel then
                     Add_Channel_To_Info
                       (Info          => Subject_Info,
                        Subject_Node  => Subj_Node,
                        Subject_Name  => Subj_Name,
                        Virt_Mem_Node => Virt_Mem_Node,
                        Phys_Mem_Node => Phys_Mem_Node);
                  else
                     Add_Memregion_To_Info
                       (Info          => Subject_Info,
                        Subject_Name  => Subj_Name,
                        Virt_Mem_Node => Virt_Mem_Node,
                        Phys_Mem_Node => Phys_Mem_Node);
                  end if;
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

            Mulog.Log (Msg => "Writing subject info data to '" & Filename
                       & "'");
            Musinfo.Writer.Serialize
              (Info     => Subject_Info,
               Filename => Filename);
         end;
      end loop;
   end Write;

end Sinfo.Generator;
