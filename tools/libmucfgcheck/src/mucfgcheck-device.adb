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

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Utils;
with Mutools.Match;

package body Mucfgcheck.Device
is

   use Ada.Strings.Unbounded;
   use McKae.XML.XPath.XIA;

   --  Returns True if the device and resource reference names match.
   function Is_Valid_Resource_Ref (Left, Right : DOM.Core.Node) return Boolean;

   --  Check that the names of specified 'Resource_Type' are unique per device.
   --  'Element_Name' specifies the name of the resource XML element.
   procedure Check_Device_Resource_Name_Uniqueness
     (XML_Data      : Muxml.XML_Data_Type;
      Resource_Type : String;
      Element_Name  : String);

   --  Check that each logical device of the given kind references a physical
   --  device given by XPath.
   procedure Match_Device_Reference
     (XML_Data            : Muxml.XML_Data_Type;
      Logical_Devs_XPath  : String;
      Physical_Devs_XPath : String;
      Device_Type         : String);

   --  Returns true if Left and Right have the same PCI device bus, device,
   --  function triplets.
   function Equal_BDFs (Left, Right : DOM.Core.Node) return Boolean;

   -------------------------------------------------------------------------

   procedure Check_Device_Resource_Name_Uniqueness
     (XML_Data      : Muxml.XML_Data_Type;
      Resource_Type : String;
      Element_Name  : String)
   is
      Devices : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device");

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

   procedure Debugconsole_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Debug_Console : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/platform/devices/device"
           & "[@name='debugconsole' and ioPort/@name='port']");
   begin
      Mulog.Log (Msg => "Checking presence of debug console device");

      if Debug_Console = null then
         raise Validation_Error with "Physical device 'debugconsole' with I/O"
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
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//device/memory[@logical]",
                      Ref_XPath    => "/system/platform/devices/device/memory",
                      Log_Message  => "device memory reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Resource_Ref'Access);
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
         if Equal_BDFs (Left  => Left,
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

   procedure Device_Sharing (XML_Data : Muxml.XML_Data_Type)
   is
      Devices : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device");
   begin
      Mulog.Log (Msg => "Checking shareability of" & DOM.Core.Nodes.Length
                 (List => Devices)'Img & " devices");
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Cur_Dev    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name   : constant String  := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Dev,
               Name => "name");
            Shareable  : constant Boolean := Boolean'Value
              (DOM.Core.Elements.Get_Attribute (Elem => Cur_Dev,
                                                Name => "shared"));
            References : constant DOM.Core.Node_List
              := XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "//devices[parent::subject|parent::kernel]/"
                 & "device[@physical='" & Dev_Name & "']");
            Ref_Count  : constant Natural := DOM.Core.Nodes.Length
              (List => References);
         begin
            if not Shareable and Ref_Count > 1 then
               declare
                  Ref_Names : Unbounded_String;
               begin
                  for J in 0 .. Ref_Count - 1 loop
                     declare
                        Cur_Ref   : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item (List  => References,
                                                  Index => J);
                        Subj_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Muxml.Utils.Ancestor_Node
                               (Node  => Cur_Ref,
                                Level => 2),
                             Name => "name");
                        Name      : constant String
                          := (if Subj_Name'Length > 0
                              then Subj_Name else "kernel");
                     begin
                        Ref_Names := Ref_Names & "'" & Name & "->"
                          & DOM.Core.Elements.Get_Attribute
                          (Elem => Cur_Ref,
                           Name => "logical") & "'";
                        if J < Ref_Count - 1 then
                           Ref_Names := Ref_Names & ", ";
                        end if;
                     end;
                  end loop;

                  raise Validation_Error with "Non-shareable device '"
                    & Dev_Name & "' is referenced by multiple logical devices "
                    & To_String (Source => Ref_Names);
               end;
            end if;
         end;
      end loop;
   end Device_Sharing;

   ----------------------------------------------------------------------

   function Equal_BDFs (Left, Right : DOM.Core.Node) return Boolean
   is
      Left_Bus  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "bus");
      Left_Dev  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "device");
      Left_Fn   : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "function");
      Right_Bus : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "bus");
      Right_Dev : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "device");
      Right_Fn  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "function");
   begin
      return Left_Bus = Right_Bus
        and then Left_Dev = Right_Dev
        and then Left_Fn  = Right_Fn;
   end Equal_BDFs;

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
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//ioPort[@logical]",
                      Ref_XPath    => "/system/platform/devices/device/ioPort",
                      Log_Message  => "I/O port reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Resource_Ref'Access);
   end IO_Port_References;

   -------------------------------------------------------------------------

   procedure IO_Port_Start_Smaller_End (XML_Data : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_32;

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device/ioPort");
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
         XPath => "/system/platform/devices/device/ioPort");

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
         XPath => "/system/platform/devices/device[capabilities/"
         & "capability/@name='iommu']/memory");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "IOMMU memory region",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end IOMMU_Region_Size;

   -------------------------------------------------------------------------

   function Is_Valid_Resource_Ref (Left, Right : DOM.Core.Node) return Boolean
   is
   begin
      return Mutools.Match.Is_Valid_Reference
        (Left  => Left,
         Right => Right)
        and then Mutools.Match.Is_Valid_Reference
          (Left  => DOM.Core.Nodes.Parent_Node (N => Left),
           Right => DOM.Core.Nodes.Parent_Node (N => Right));
   end Is_Valid_Resource_Ref;

   -------------------------------------------------------------------------

   procedure Legacy_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Match_Device_Reference
        (XML_Data            => XML_Data,
         Logical_Devs_XPath  => "/system/subjects/subject/devices"
         & "/device[not (pci)]",
         Physical_Devs_XPath => "/system/platform/devices/device[not (pci)]",
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
         XPath => "/system/platform/devices/device/pci");

      --  Check inequality of PCI device bus, device, function triplets.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
      begin
         if Equal_BDFs (Left  => Left,
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
         Physical_Devs_XPath => "/system/platform/devices/device[pci]",
         Device_Type         => "PCI");
   end PCI_Device_References;

   -------------------------------------------------------------------------

   procedure Physical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device");

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
            raise Validation_Error with "Multiple physical devices with name '"
              & Left_Name & "'";
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " physical device name(s)");

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
                      Ref_XPath    => "/system/platform/devices/device",
                      Log_Message  => "physical device reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Mutools.Match.Is_Valid_Reference'Access);
   end Physical_Device_References;

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
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//irq[@logical]",
                      Ref_XPath    => "/system/platform/devices/device/irq",
                      Log_Message  => "device IRQ reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Resource_Ref'Access);
   end Physical_IRQ_References;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device/irq");

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
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " device IRQ(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Physical_IRQ_Uniqueness;

end Mucfgcheck.Device;
