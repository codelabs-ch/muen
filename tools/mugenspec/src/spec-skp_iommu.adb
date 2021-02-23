--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Match;
with Mutools.XML_Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_IOMMU
is

   use Ada.Strings.Unbounded;

   --  Generate body case statements.
   procedure Generate_Body_Case_Statements
     (Template : in out Mutools.Templates.Template_Type;
      Count    :        Positive);

   --  Generate variable record field offset and size constants.
   procedure Generate_Variable_Offsets_Sizes
     (Template : in out Mutools.Templates.Template_Type;
      IOMMUs   :        DOM.Core.Node_List;
      Count    :        Positive);

   --  Generate IOMMU record types and representation clauses.
   procedure Generate_IOMMU_Record_Types
     (Template : in out Mutools.Templates.Template_Type;
      Count    :        Positive);

   -------------------------------------------------------------------------

   procedure Generate_Body_Case_Statements
     (Template : in out Mutools.Templates.Template_Type;
      Count    :        Positive)
   is
      function U
        (S : String)
         return Unbounded_String
         renames To_Unbounded_String;

      --  Create case statement for given record variable.
      function Create_Case
        (Var    : String;
         Common : Boolean;
         Write  : Boolean)
         return String;

      ----------------------------------------------------------------------

      function Create_Case
        (Var    : String;
         Common : Boolean;
         Write  : Boolean)
         return String
      is
         Res : Unbounded_String;
      begin
         for I in 1 .. Count loop
            Res := Res & Indent (N => 3) & "when" & I'Img & " => "
              & (if not Write then "Value := " else "") & "IOMMUs.IOMMU_"
              & Ada.Strings.Fixed.Trim (Source => I'Img,
                                        Side   => Ada.Strings.Left)
              & (if Common then ".Common." else ".")
              & Var
              & (if Write then " := Value" else "")
              & (if I < Count then ";" & ASCII.LF else ";");
         end loop;

         return To_String (Res);
      end Create_Case;

      type Entry_Type is record
         Replace  : Unbounded_String;
         Variable : Unbounded_String;
         Common   : Boolean;
         Write    : Boolean;
      end record;

      To_Replace : constant array (Positive range <>) of Entry_Type
        := (1  => (Replace  => U ("__body_read_capability_case__"),
                   Variable => U ("Capability"),
                   Common   => True,
                   Write    => False),
            2  => (Replace  => U ("__body_read_context_command_case__"),
                   Variable => U ("Context_Command"),
                   Common   => True,
                   Write    => False),
            3  => (Replace  => U ("__body_read_extended_capability_case__"),
                   Variable => U ("Ext_Capability"),
                   Common   => True,
                   Write    => False),
            4  => (Replace  => U ("__body_read_fault_event_address_case__"),
                   Variable => U ("Fault_Event_Address"),
                   Common   => True,
                   Write    => False),
            5  => (Replace  => U ("__body_read_fault_event_control_case__"),
                   Variable => U ("Fault_Event_Control"),
                   Common   => True,
                   Write    => False),
            6  => (Replace  => U ("__body_read_fault_event_data_case__"),
                   Variable => U ("Fault_Event_Data"),
                   Common   => True,
                   Write    => False),
            7  => (Replace  => U ("__body_read_fault_recording_case__"),
                   Variable => U ("Fault_Recording"),
                   Common   => False,
                   Write    => False),
            8  => (Replace  => U ("__body_read_fault_status_case__"),
                   Variable => U ("Fault_Status"),
                   Common   => True,
                   Write    => False),
            9  => (Replace  => U ("__body_read_global_status_case__"),
                   Variable => U ("Global_Status"),
                   Common   => True,
                   Write    => False),
            10 => (Replace  => U ("__body_read_iotlb_invalidate_case__"),
                   Variable => U ("IOTLB_Invalidate"),
                   Common   => False,
                   Write    => False),
            11 => (Replace  => U ("__body_read_version_case__"),
                   Variable => U ("Version"),
                   Common   => True,
                   Write    => False),
            12 => (Replace  => U ("__body_write_context_command_case__"),
                   Variable => U ("Context_Command"),
                   Common   => True,
                   Write    => True),
            13 => (Replace  => U ("__body_write_fault_event_address_case__"),
                   Variable => U ("Fault_Event_Address"),
                   Common   => True,
                   Write    => True),
            14 => (Replace  => U ("__body_write_fault_event_control_case__"),
                   Variable => U ("Fault_Event_Control"),
                   Common   => True,
                   Write    => True),
            15 => (Replace  => U ("__body_write_fault_event_data_case__"),
                   Variable => U ("Fault_Event_Data"),
                   Common   => True,
                   Write    => True),
            16 => (Replace  => U ("__body_write_fault_recording_case__"),
                   Variable => U ("Fault_Recording"),
                   Common   => False,
                   Write    => True),
            17 => (Replace  => U ("__body_write_fault_status_case__"),
                   Variable => U ("Fault_Status"),
                   Common   => True,
                   Write    => True),
            18 => (Replace  => U ("__body_write_global_command_case__"),
                   Variable => U ("Global_Command"),
                   Common   => True,
                   Write    => True),
            19 => (Replace  => U ("__body_write_iotlb_invalidate_case__"),
                   Variable => U ("IOTLB_Invalidate"),
                   Common   => False,
                   Write    => True),
            20 => (Replace  => U ("__body_write_irt_address_case__"),
                   Variable => U ("IRT_Address"),
                   Common   => True,
                   Write    => True),
            21 => (Replace  => U ("__body_write_root_table_address_case__"),
                   Variable => U ("Root_Table_Address"),
                   Common   => True,
                   Write    => True));
   begin
      for R of To_Replace loop
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => To_String (R.Replace),
            Content  => Create_Case (Var    => To_String (R.Variable),
                                     Common => R.Common,
                                     Write  => R.Write));
      end loop;
   end Generate_Body_Case_Statements;

   -------------------------------------------------------------------------

   procedure Generate_IOMMU_Record_Types
     (Template : in out Mutools.Templates.Template_Type;
      Count    :        Positive)
   is
      IOMMU_X_String, IOMMU_X_Repr_String, IOMMU_Fields : Unbounded_String;
   begin
      for I in 1 .. Count loop
         declare
            Suffix : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            IOMMU_X_String := IOMMU_X_String & Indent (N => 1) & "type IOMMU_"
              & Suffix & "_Type is new IOMMU_X_Type with Size => IOMMU_"
              & Suffix & "_Type_Size"
              & (if I < Count then ";" & ASCII.LF else ";");

            IOMMU_X_Repr_String := IOMMU_X_Repr_String & Indent (N => 1)
              & "for IOMMU_" & Suffix & "_Type use record" & ASCII.LF
              & Indent (N => 2) & "Common at 0 range 0 .. "
              & "IOMMU_Common_Size - 1;" & ASCII.LF
              & Indent (N => 2) & "IOTLB_Invalidate at IOTLB_Inv_Offset_"
              & Suffix & " range 0 .. 63;" & ASCII.LF
              & Indent (N => 2) & "Fault_Recording at FR_Offset_" & Suffix
              & " range 0 .. 127;" & ASCII.LF
              & Indent (N => 1) & "end record"
              & (if I < Count then ";" & ASCII.LF & ASCII.LF else ";");

            IOMMU_Fields := IOMMU_Fields & Indent (N => 2) & "IOMMU_" & Suffix
              & " : IOMMU_" & Suffix & "_Type;" & ASCII.LF
              & Indent (N => 2) & "Padding_" & Suffix & " : Bit_Array "
              & "(1 .. SK.Page_Size * 8 - IOMMU_" & Suffix & "_Type_Size)"
              & (if I < Count then ";" & ASCII.LF else ";");
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iommu_x_types__",
         Content  => To_String (IOMMU_X_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iommu_x_types_repr__",
         Content  => To_String (IOMMU_X_Repr_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iommu_record_fields__",
         Content  => To_String (IOMMU_Fields));
   end Generate_IOMMU_Record_Types;

   -------------------------------------------------------------------------

   procedure Generate_Variable_Offsets_Sizes
     (Template : in out Mutools.Templates.Template_Type;
      IOMMUs   :        DOM.Core.Node_List;
      Count    :        Positive)
   is
      Iotlb_String, Iotlb_Array_String, Fro_String, Fro_Array_String,
      Sizes_String : Unbounded_String;
   begin
      Iotlb_Array_String := Iotlb_Array_String & Indent (N => 1) & "  := ("
        & ASCII.LF;
      Fro_Array_String := Fro_Array_String & Indent (N => 1) & "  := ("
        & ASCII.LF;

      for I in 1 .. Count loop
         declare
            --  Intel VT-d Specification, "10.4.8.1 IOTLB Invalidate Register"
            IOTLB_Invalidate_Size_Bits : constant := 64;
            --  Intel VT-d Specification, "10.4.14 Fault Recording
            --  Registers [n]"
            Fault_Recording_Size_Bits  : constant := 128;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => IOMMUs,
                                      Index => I - 1);
            Fro_Cap : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Node,
                 XPath => "capabilities/capability[@name='fr_offset']");
            Fro_Cap_Val : constant Positive
              := Positive'Value (Fro_Cap);
            Iotlb_Inv_Cap : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Node,
                 XPath => "capabilities/capability"
                 & "[@name='iotlb_invalidate_offset']");
            Iotlb_Inv_Cap_Val : constant Positive
              := Positive'Value (Iotlb_Inv_Cap);
            Suffix : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Larger_Offset : constant Positive
              := Positive'Max (Iotlb_Inv_Cap_Val, Fro_Cap_Val);
            Register_Size : constant Positive
              := (if Iotlb_Inv_Cap_Val > Fro_Cap_Val
                  then IOTLB_Invalidate_Size_Bits
                  else Fault_Recording_Size_Bits);
         begin
            Iotlb_String := Iotlb_String & Indent (N => 1)
              & "IOTLB_Inv_Offset_" & Suffix & " : constant := "
              & Iotlb_Inv_Cap & (if I < Count then ";" & ASCII.LF else ";");
            Fro_String := Fro_String & Indent (N => 1) & "FR_Offset_" & Suffix
              & " : constant := " & Fro_Cap
              & (if I < Count then ";" & ASCII.LF else ";");

            Iotlb_Array_String := Iotlb_Array_String & Indent (N => 3)
              & Suffix & " => IOTLB_Inv_Offset_" & Suffix
              & (if I < Count then "," & ASCII.LF
                 else ASCII.LF & Indent (N => 2) & "  );");
            Fro_Array_String := Fro_Array_String & Indent (N => 3)
              & Suffix & " => FR_Offset_" & Suffix
              & (if I < Count then "," & ASCII.LF
                 else ASCII.LF & Indent (N => 2) & "  );");

            Sizes_String := Sizes_String & Indent (N => 1) & "IOMMU_" & Suffix
              & "_Type_Size : constant := 8 *" & Larger_Offset'Img & " +"
              & Register_Size'Img
              & (if I < Count then ";" & ASCII.LF else ";");
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iotlb_inv_offsets__",
         Content  => To_String (Iotlb_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iotlb_inv_offset_array__",
         Content  => To_String (Iotlb_Array_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__fr_offsets__",
         Content  => To_String (Fro_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__fr_offset_array__",
         Content  => To_String (Fro_Array_String));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__iommu_type_sizes__",
         Content  => To_String (Sizes_String));
   end Generate_Variable_Offsets_Sizes;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      --  Return the lowest virtualAddress value string of the memory regions
      --  given as node list.
      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String;

      ----------------------------------------------------------------------

      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String
      is
         Result : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
         Count  : constant Natural       := DOM.Core.Nodes.Length
           (List => Nodes);
      begin
         for I in 0 .. Count - 1 loop
            declare
               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I);
               virtualAddr : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => "virtualAddress"));
            begin
               if virtualAddr < Result then
                  Result := virtualAddr;
               end if;
            end;
         end loop;

         return Mutools.Utils.To_Hex (Number => Result);
      end Get_Base_Addr;

      Filename  : constant String := Output_Dir & "/skp-iommu.ads";
      Phys_Mem  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Root_Addr : constant String
        := Muxml.Utils.Get_Attribute (Nodes     => Phys_Mem,
                                      Ref_Attr  => "type",
                                      Ref_Value => "system_vtd_root",
                                      Attr_Name => "physicalAddress");
      IRT_Phys_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute (Nodes     => Phys_Mem,
                                      Ref_Attr  => "type",
                                      Ref_Value => "system_vtd_ir",
                                      Attr_Name => "physicalAddress");
      IRT_Phys_Addr : Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value (IRT_Phys_Addr_Str);
      IOMMUs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => Policy,
           Left_XPath  => "/system/kernel/devices/device/"
           & "memory[@logical='mmio']",
           Right_XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference_Lparent'Access);
      IOMMU_Count : constant Positive := DOM.Core.Nodes.Length
        (List => IOMMUs.Right);
      IOMMU_PT_Levels : constant Mutools.XML_Utils.IOMMU_Paging_Level
        := Mutools.XML_Utils.Get_IOMMU_Paging_Levels (Data => Policy);
      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing IOMMU spec to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_ads);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__root_table_addr__",
         Content  => (if Root_Addr'Length > 0 then Root_Addr else "0"));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__iommu_device_range__",
         Content  => "1 .." & IOMMU_Count'Img);

      --  Shifted, 4KB aligned IR table address (see Intel VT-d Specification,
      --  "10.4.30 Interrupt Remapping Table Address Register").

      IRT_Phys_Addr := IRT_Phys_Addr / 2 ** 12;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ir_table_phys_addr__",
         Content  => Mutools.Utils.To_Hex (Number => IRT_Phys_Addr));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cap_agaw_bit__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Positive'Image (IOMMU_PT_Levels - 1),
            Side   => Ada.Strings.Left));

      Generate_Variable_Offsets_Sizes
        (Template => Tmpl,
         IOMMUs   => IOMMUs.Right,
         Count    => IOMMU_Count);
      Generate_IOMMU_Record_Types
        (Template => Tmpl,
         Count    => IOMMU_Count);

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_adb);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__base_addr__",
         Content  => Get_Base_Addr (Nodes => IOMMUs.Left));

      Generate_Body_Case_Statements
        (Template => Tmpl,
         Count    => IOMMU_Count);

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-iommu.adb");
   end Write;

end Spec.Skp_IOMMU;
