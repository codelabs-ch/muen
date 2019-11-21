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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Utils;
with Mutools.Match;
with Mutools.XML_Utils;

with Mucfgcheck.Utils;

package body Mucfgcheck.Device
is

   use McKae.XML.XPath.XIA;

   --  Check that the names of specified 'Resource_Type' are unique per device.
   --  'Element_Name' specifies the name of the resource XML element.
   procedure Check_Device_Resource_Name_Uniqueness
     (XML_Data      : Muxml.XML_Data_Type;
      Resource_Type : String;
      Element_Name  : String);

   --  Check that IRQs of devices given by XPath query string fulfill the
   --  specified count and range constraints.
   procedure Check_IRQ_Constraints
     (XML_Data    : Muxml.XML_Data_Type;
      Dev_XPath   : String;
      IRQ_Kind    : String;
      Count       : Positive;
      Range_Start : Interfaces.Unsigned_64;
      Range_End   : Interfaces.Unsigned_64);

   --  Check that each logical device of the given kind references a physical
   --  device given by XPath.
   procedure Match_Device_Reference
     (XML_Data            : Muxml.XML_Data_Type;
      Logical_Devs_XPath  : String;
      Physical_Devs_XPath : String;
      Device_Type         : String);

   -------------------------------------------------------------------------

   procedure Check_Device_Resource_Name_Uniqueness
     (XML_Data      : Muxml.XML_Data_Type;
      Resource_Type : String;
      Element_Name  : String)
   is
      Devices : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device");

      --  Check device resource names.
      procedure Check_Names (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Names (Left, Right : DOM.Core.Node)
      is
         Dev_Name   : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
            Name => "name");
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Device '" & Dev_Name & "' has"
              & " multiple " & Resource_Type & "s with name '"
              & Left_Name & "'";
         end if;
      end Check_Names;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Cur_Dev   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Dev,
                 Name => "name");
            Resources : constant DOM.Core.Node_List := XPath_Query
              (N     => Cur_Dev,
               XPath => Element_Name);
            Res_Count : constant Natural := DOM.Core.Nodes.Length
              (List => Resources);
         begin
            if Res_Count > 1 then
               Mulog.Log (Msg => "Checking uniqueness of" & Res_Count'Img
                          & " " & Resource_Type & " name(s) of device '"
                          & Dev_Name & "'");
               Compare_All (Nodes      => Resources,
                            Comparator => Check_Names'Access);
            end if;
         end;
      end loop;
   end Check_Device_Resource_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Check_IRQ_Constraints
     (XML_Data    : Muxml.XML_Data_Type;
      Dev_XPath   : String;
      IRQ_Kind    : String;
      Count       : Positive;
      Range_Start : Interfaces.Unsigned_64;
      Range_End   : Interfaces.Unsigned_64)
   is
      Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Dev_XPath);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Dev      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev,
                 Name => "name");
            IRQs     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev,
                 XPath => "irq");
            Length   : constant Natural
              := DOM.Core.Nodes.Length (List => IRQs);
         begin
            if Length > Count then
               raise Validation_Error with "Device '" & Dev_Name & "' "
                 & "specifies more than" & Count'Img & " " & IRQ_Kind
                 & " IRQ(s)";
            elsif Length /= 0 then
               Check_Attribute
                 (Nodes     => IRQs,
                  Node_Type => IRQ_Kind & " IRQ",
                  Attr      => "number",
                  Name_Attr => "name",
                  Test      => In_Range'Access,
                  B         => Range_Start,
                  C         => Range_End,
                  Error_Msg => "not in allowed range" & Range_Start'Img & " .."
                  & Range_End'Img & " (device '" & Dev_Name & "')");
            end if;
         end;
      end loop;
   end Check_IRQ_Constraints;

   -------------------------------------------------------------------------

   procedure Debugconsole_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Debug_Console : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/kernel/devices/device"
           & "[@logical='debugconsole' and ioPort/@logical='port']");
   begin
      Mulog.Log (Msg => "Checking presence of kernel debug console device");

      if Debug_Console = null then
         raise Validation_Error with "Kernel device 'debugconsole' with I/O"
           & " port resource 'port' not found";
      end if;
   end Debugconsole_Presence;

   -------------------------------------------------------------------------

   procedure Device_IO_Port_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Device_Resource_Name_Uniqueness
        (XML_Data      => XML_Data,
         Resource_Type => "I/O port",
         Element_Name  => "ioPort");
   end Device_IO_Port_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Device_IRQ_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Device_Resource_Name_Uniqueness
        (XML_Data      => XML_Data,
         Resource_Type => "IRQ",
         Element_Name  => "irq");
   end Device_IRQ_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Device_Memory_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Device_Resource_Name_Uniqueness
        (XML_Data      => XML_Data,
         Resource_Type => "memory region",
         Element_Name  => "memory");
   end Device_Memory_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Device_Memory_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Log_Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "logical");
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Phys_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical device memory '" & Phys_Name
           & "' referenced by logical device memory '" & Logical_Name
           & "' of logical device '" & Log_Dev_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "//device/memory[@logical]",
         Ref_XPath    => "/system/hardware/devices/device/memory",
         Log_Message  => "device memory reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Resource_Ref'Access);

      Performed_Checks.Insert
        (New_Item => Device_Memory_References'Address);
   end Device_Memory_References;

   -------------------------------------------------------------------------

   procedure Device_Reference_BDF_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      PCI_Subjs   : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject[devices/device/pci]");

      --  Node of current subject.
      Cur_Subject : DOM.Core.Node;

      --  Check inequality of PCI device reference bus, device, function
      --   triplets.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
      begin
         if Mutools.XML_Utils.Equal_BDFs
           (Left  => Left,
            Right => Right)
         then
            declare
               Left_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
                    Name => "logical");
               Right_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Right),
                    Name => "logical");
               Subj_Name  : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Subject,
                  Name => "name");
            begin
               raise Validation_Error with "Logical PCI devices '" & Left_Name
                 & "' and '" & Right_Name & "' of subject '"
                 & Subj_Name & "' have identical BDF";
            end;
         end if;
      end Check_Inequality;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => PCI_Subjs) - 1 loop
         Cur_Subject := DOM.Core.Nodes.Item (List  => PCI_Subjs,
                                             Index => I);
         declare
            PCI_Devs  : constant DOM.Core.Node_List := XPath_Query
              (N     => Cur_Subject,
               XPath => "devices/device/pci");
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Subject,
               Name => "name");
         begin
            if DOM.Core.Nodes.Length (List => PCI_Devs) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => PCI_Devs)'Img
                          & " device reference BDF(s) of subject '"
                          & Subj_Name & "'");

               Compare_All (Nodes      => PCI_Devs,
                            Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Device_Reference_BDF_Uniqueness;

   -------------------------------------------------------------------------

   procedure Device_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Subjs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject");
      Subj_Count : constant Natural := DOM.Core.Nodes.Length (List => Subjs);

      --  Node of current subject.
      Cur_Subject : DOM.Core.Node;

      --  Check inequality of device reference.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Physical  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "physical");
         Right_Physical : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "physical");
      begin
         if Left_Physical = Right_Physical then
            declare
               Left_Logical  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Left,
                    Name => "logical");
               Right_Logical : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Right,
                    Name => "logical");
               Subj_Name  : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Subject,
                  Name => "name");
            begin
               raise Validation_Error with "Logical devices '" & Left_Logical
                 & "' and '" & Right_Logical & "' of subject '" & Subj_Name
                 & "' reference same physical device '" & Left_Physical & "'";
            end;
         end if;
      end Check_Inequality;
   begin
      for I in 0 .. Subj_Count - 1 loop
         Cur_Subject := DOM.Core.Nodes.Item (List  => Subjs,
                                             Index => I);
         declare
            Logical_Devices : constant DOM.Core.Node_List := XPath_Query
              (N     => Cur_Subject,
               XPath => "devices/device");
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Subject,
               Name => "name");
         begin
            if DOM.Core.Nodes.Length (List => Logical_Devices) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Logical_Devices)'Img
                          & " device references of subject '"
                          & Subj_Name & "'");

               Compare_All (Nodes      => Logical_Devices,
                            Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Device_Reference_Uniqueness;

   -------------------------------------------------------------------------

   procedure Device_References_PCI_Bus_Number (XML_Data : Muxml.XML_Data_Type)
   is
      PCI_Dev_Refs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject/devices/device/pci");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => PCI_Dev_Refs) - 1 loop
         declare
            PCI_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => PCI_Dev_Refs,
                 Index => I);
            Bus_Nr   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PCI_Node,
                    Name => "bus"));
         begin
            if Bus_Nr /= 0 then
               declare
                  Log_Dev_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => PCI_Node),
                       Name => "logical");
                  Subj_Name    : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => PCI_Node,
                          Level => 3),
                       Name => "name");
               begin
                  raise Validation_Error with "Logical PCI device '"
                    & Log_Dev_Name & "' of subject '" & Subj_Name
                    & "' specifies invalid bus number "
                    & Mutools.Utils.To_Hex
                    (Number     => Bus_Nr,
                     Normalize  => True,
                     Byte_Short => True)
                    & " should be 16#00#";
               end;
            end if;
         end;
      end loop;
   end Device_References_PCI_Bus_Number;

   -------------------------------------------------------------------------

   procedure IO_Port_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Log_Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "logical");
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Phys_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical I/O port '" & Phys_Name
           & "' referenced by logical I/O port '" & Logical_Name
           & "' of logical device '" & Log_Dev_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "//ioPort[@logical]",
         Ref_XPath    => "/system/hardware/devices/device/ioPort",
         Log_Message  => "I/O port reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Resource_Ref'Access);
   end IO_Port_References;

   -------------------------------------------------------------------------

   procedure IO_Port_Start_Smaller_End (XML_Data : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_32;

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device/ioPort");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " I/O port range(s) for start <= end");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            S_Addr_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "start");
            S_Addr     : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (S_Addr_Str);
            E_Addr_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "end");
            E_Addr     : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (E_Addr_Str);

            --  Either name or logical attribute exists.

            Name       : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "name");
            Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical");
         begin
            if S_Addr > E_Addr then
               raise Validation_Error with "I/O port '" & Name & Logical_Name
                 & "' start " & S_Addr_Str & " larger than end " & E_Addr_Str;
            end if;
         end;
      end loop;
   end IO_Port_Start_Smaller_End;

   -------------------------------------------------------------------------

   procedure IO_Port_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device/ioPort");

      --  Check inequality/non-overlap of I/O port ranges.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Start  : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "start"));
         Left_End    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "end"));
         Right_Start : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "start"));
         Right_End   : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "end"));
      begin
         if (Left_Start <= Right_Start and then Left_End >= Right_Start)
           or (Right_Start <= Left_Start and then Right_End >= Left_Start)
         then
            declare
               Left_Dev_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                 (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
                  Name => "name");
               Right_Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Right),
                    Name => "name");
            begin
               raise Validation_Error with "Devices '" & Left_Dev_Name
                 & "' and '" & Right_Dev_Name & "' have overlapping I/O "
                 & "port(s)";
            end;
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " device I/O port(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end IO_Port_Uniqueness;

   -------------------------------------------------------------------------

   procedure IOMMU_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device[capabilities/"
         & "capability/@name='iommu']/memory");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "IOMMU memory region",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end IOMMU_Region_Size;

   -------------------------------------------------------------------------

   procedure Legacy_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Match_Device_Reference
        (XML_Data            => XML_Data,
         Logical_Devs_XPath  => "/system/subjects/subject/devices"
         & "/device[not (pci)]",
         Physical_Devs_XPath => "/system/hardware/devices/device[not (pci)]",
         Device_Type         => "legacy");
   end Legacy_Device_References;

   -------------------------------------------------------------------------

   procedure Match_Device_Reference
     (XML_Data            : Muxml.XML_Data_Type;
      Logical_Devs_XPath  : String;
      Physical_Devs_XPath : String;
      Device_Type         : String)
   is
      Physical_Devs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => Physical_Devs_XPath);
      Logical_Devs  : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => Logical_Devs_XPath);
      Log_Count     : constant Natural
        := DOM.Core.Nodes.Length (List => Logical_Devs);
   begin
      if Log_Count > 1 then
         Mulog.Log (Msg => "Checking" & Log_Count'Img
                    & " " & Device_Type & " device reference(s)");

         for I in 0 .. Log_Count - 1 loop
            declare
               use type DOM.Core.Node;

               Dev_Ref   : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Logical_Devs,
                                         Index => I);
               Subj_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Muxml.Utils.Ancestor_Node
                      (Node  => Dev_Ref,
                       Level => 2),
                    Name => "name");
               Log_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Ref,
                    Name => "logical");
               Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Ref,
                    Name => "physical");
               Phys_Dev  : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Physical_Devs,
                    Ref_Attr  => "name",
                    Ref_Value => Phys_Name);
            begin
               if Phys_Dev = null then
                  raise Validation_Error with "Logical " & Device_Type
                    & " device '" & Log_Name & "' of subject '" & Subj_Name
                    & "' references physical non-" & Device_Type
                    & " device '" & Phys_Name & "'";
               end if;
            end;
         end loop;
      end if;
   end Match_Device_Reference;

   -------------------------------------------------------------------------

   procedure PCI_Device_BDF_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device/pci");

      --  Check inequality of PCI device bus, device, function triplets.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
      begin
         if Mutools.XML_Utils.Equal_BDFs
           (Left  => Left,
            Right => Right)
         then
            declare
               Left_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
                    Name => "name");
               Right_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Right),
                    Name => "name");
            begin
               raise Validation_Error with "PCI devices '" & Left_Name
                 & "' and '" & Right_Name & "' have identical BDF";
            end;
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " PCI device BDF(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end PCI_Device_BDF_Uniqueness;

   -------------------------------------------------------------------------

   procedure PCI_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Match_Device_Reference
        (XML_Data            => XML_Data,
         Logical_Devs_XPath  => "/system/subjects/subject/devices/device[pci]",
         Physical_Devs_XPath => "/system/hardware/devices/device[pci]",
         Device_Type         => "PCI");
   end PCI_Device_References;

   -------------------------------------------------------------------------

   procedure PCI_Multifunction_Device_Refs (XML_Data : Muxml.XML_Data_Type)
   is
      Log_PCI_Devs    : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/device[pci]");
      Sorted_PCI_Devs : constant DOM.Core.Node_List
        := Mutools.XML_Utils.Sort_By_BDF (PCI_Devs => XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[pci]"));

      Prev_Bus, Prev_Dev, Prev_Log_Dev         : Natural       := Natural'Last;
      Prev_Subj, Prev_Phys_Node, Prev_Log_Node : DOM.Core.Node := null;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Sorted_PCI_Devs) - 1
      loop
         declare
            use type DOM.Core.Node;

            Phys_Dev  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Sorted_PCI_Devs,
                 Index => I);
            PCI_Node  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element (Doc   => Phys_Dev,
                                          XPath => "pci");
            Bus_Nr    : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => PCI_Node,
                  Name => "bus"));
            Dev_Nr    : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => PCI_Node,
                  Name => "device"));
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Dev,
                 Name => "name");
            Log_Dev   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Log_PCI_Devs,
                 Ref_Attr  => "physical",
                 Ref_Value => Phys_Name);
            Subject   : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => Log_Dev,
                                            Level => 2);
         begin
            if Log_Dev /= null then

               --  Only consider devices that are actually referenced.

               declare
                  Log_Dev_Nr : constant Natural := Natural'Value
                    (Muxml.Utils.Get_Attribute
                       (Doc   => Log_Dev,
                        XPath => "pci",
                        Name  => "device"));
               begin
                  if Bus_Nr = Prev_Bus and then Dev_Nr = Prev_Dev then
                     if Subject /= Prev_Subj then
                        declare
                           Prev_Phys_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Prev_Phys_Node,
                                Name => "name");
                        begin
                           raise Validation_Error with "Physical devices '"
                             & Prev_Phys_Name & "' and '" & Phys_Name & "' are"
                             & " part of a PCI multi-function device and must "
                             & "be assigned to the same subject";
                        end;
                     end if;

                     if Log_Dev_Nr /= Prev_Log_Dev then
                        declare
                           Log_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Log_Dev,
                                Name => "logical");
                           Prev_Log_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Prev_Log_Node,
                                Name => "logical");
                        begin
                           raise Validation_Error with "Logical devices '"
                             & Prev_Log_Name & "' and '" & Log_Name & "' are"
                             & " part of a PCI multi-function device and must"
                             & " have the same logical device number: "
                             & Mutools.Utils.To_Hex
                             (Number     => Interfaces.Unsigned_64
                                (Prev_Log_Dev),
                              Byte_Short => True) & " /= "
                             & Mutools.Utils.To_Hex
                             (Number     => Interfaces.Unsigned_64
                                (Log_Dev_Nr),
                              Byte_Short => True);
                        end;
                     end if;
                  end if;

                  Prev_Bus       := Bus_Nr;
                  Prev_Dev       := Dev_Nr;
                  Prev_Subj      := Subject;
                  Prev_Log_Dev   := Log_Dev_Nr;
                  Prev_Phys_Node := Phys_Dev;
                  Prev_Log_Node  := Log_Dev;
               end;
            end if;
         end;
      end loop;
   end PCI_Multifunction_Device_Refs;

   -------------------------------------------------------------------------

   procedure Physical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Devs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device");
      Aliases   : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/mappings/aliases/alias");
      Classes   : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/mappings/classes/class");

      Nodes     : DOM.Core.Node_List;

      --  Check inequality of device names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Multiple physical devices, aliases or"
              & " classes with name '" & Left_Name & "'";
         end if;
      end Check_Inequality;
   begin
      Muxml.Utils.Append (Left  => Nodes,
                          Right => Phys_Devs);
      Muxml.Utils.Append (Left  => Nodes,
                          Right => Aliases);
      Muxml.Utils.Append (Left  => Nodes,
                          Right => Classes);

      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " device name(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Physical_Device_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Physical_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Phys_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical device '" & Phys_Name & "' referenced by logical"
           & " device '" & Logical_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//device[@physical]",
                      Ref_XPath    => "/system/hardware/devices/device",
                      Log_Message  => "physical device reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Mutools.Match.Is_Valid_Reference'Access);

      Performed_Checks.Insert
        (New_Item => Physical_Device_References'Address);
   end Physical_Device_References;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Constraints_ISA (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_IRQ_Constraints
        (XML_Data    => XML_Data,
         Dev_XPath   => "/system/hardware/devices/device[not(pci) and irq]",
         IRQ_Kind    => "ISA",
         Count       => 16,
         Range_Start => 0,
         Range_End   => 15);
   end Physical_IRQ_Constraints_ISA;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Constraints_PCI_LSI (XML_Data : Muxml.XML_Data_Type)
   is
      Max_LSI : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Mutools.XML_Utils.Get_IOAPIC_RTE_Index_Max
                                   (Data => XML_Data));
   begin
      Check_IRQ_Constraints
        (XML_Data    => XML_Data,
         Dev_XPath   => "/system/hardware/devices/device"
         & "[pci/@msi='false' and irq]",
         IRQ_Kind    => "PCI LSI",
         Count       => 4,
         Range_Start => 0,
         Range_End   => Max_LSI);
   end Physical_IRQ_Constraints_PCI_LSI;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Constraints_PCI_MSI (XML_Data : Muxml.XML_Data_Type)
   is
      package MC renames Mutools.Constants;

      Max_LSI : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Mutools.XML_Utils.Get_IOAPIC_RTE_Index_Max
                                   (Data => XML_Data));
   begin
      Check_IRQ_Constraints
        (XML_Data    => XML_Data,
         Dev_XPath   => "/system/hardware/devices/device"
         & "[pci/@msi='true' and irq]",
         IRQ_Kind    => "PCI MSI",
         Count       => MC.Hardware_Max_IRQ_Number - Positive (Max_LSI),
         Range_Start => Max_LSI + 1,
         Range_End   => MC.Hardware_Max_IRQ_Number);
   end Physical_IRQ_Constraints_PCI_MSI;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_MSI_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      Devices : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device"
           & "[pci/@msi='true' and count(irq) > 1]");

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the left and right numbers are adjacent.
      function Is_Adjacent_Number (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
         IRQ_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
      begin
         return "MSI IRQ '" & IRQ_Name & "' of physical device '" & Dev_Name
           & "' not adjacent to other IRQs";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Is_Adjacent_Number (Left, Right : DOM.Core.Node) return Boolean
      is
      begin
         return Utils.Is_Adjacent_Number
           (Left  => Left,
            Right => Right,
            Attr  => "number");
      end Is_Adjacent_Number;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev,
                 Name => "name");
            IRQs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev,
                 XPath => "irq");
         begin
            For_Each_Match
              (Source_Nodes => IRQs,
               Ref_Nodes    => IRQs,
               Log_Message  => "PCI MSI IRQs of device '" & Dev_Name
               & "' for consecutiveness",
               Error        => Error_Msg'Access,
               Match        => Is_Adjacent_Number'Access);
         end;
      end loop;
   end Physical_IRQ_MSI_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Log_Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "logical");
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Phys_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical IRQ '" & Phys_Name & "' referenced by logical IRQ '"
           & Logical_Name & "' of logical device '" & Log_Dev_Name
           & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "//irq[@logical]",
         Ref_XPath    => "/system/hardware/devices/device/irq",
         Log_Message  => "device IRQ reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Resource_Ref'Access);
   end Physical_IRQ_References;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => XML_Data,
           Left_XPath  => "//irq[@logical]",
           Right_XPath => "/system/hardware/devices/device/irq",
           Match       => Mutools.Match.Is_Valid_Resource_Ref'Access);

      IRQ_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Nodes.Right);

      --  Check inequality of IRQ numbers.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Number    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "number"));
         Left_Dev_Name  : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
            Name => "name");
         Right_Number   : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "number"));
         Right_Dev_Name : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Right),
            Name => "name");
      begin
         if Left_Number = Right_Number then
            raise Validation_Error with "Devices '" & Left_Dev_Name & "' and '"
              & Right_Dev_Name & "' share IRQ" & Left_Number'Img;
         end if;
      end Check_Inequality;
   begin
      if IRQ_Count < 2 then
         return;
      end if;

      Mulog.Log (Msg => "Checking uniqueness of" & IRQ_Count'Img
                 & " device IRQ(s)");
      Compare_All (Nodes      => Nodes.Right,
                   Comparator => Check_Inequality'Access);
   end Physical_IRQ_Uniqueness;

end Mucfgcheck.Device;
