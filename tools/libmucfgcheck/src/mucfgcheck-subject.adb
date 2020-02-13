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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Append_Node;
with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Types;
with Mutools.XML_Utils;
with Mutools.Match;
with Mutools.Constants;

with Mucfgcheck.Utils;

package body Mucfgcheck.Subject
is

   use Ada.Strings.Unbounded;
   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure CPU_ID (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Last_ID   : constant Natural := CPU_Count - 1;
      Nodes     : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "subject",
                       Attr      => "cpu",
                       Name_Attr => "name",
                       Test      => Less_Or_Equal'Access,
                       B         => Interfaces.Unsigned_64 (Last_ID),
                       Error_Msg => "not in valid range 0 .." & Last_ID'Img);
   end CPU_ID;

   -------------------------------------------------------------------------

   procedure Crash_Audit_Write_Access (XML_Data : Muxml.XML_Data_Type)
   is
      Pairs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data       => XML_Data,
           Left_XPath     => "/system/subjects/subject/memory/memory"
           & "[@writable='true']",
           Right_XPath    => "/system/memory/memory"
           & "[@type='subject_crash_audit']",
           Match_Multiple => False,
           Match          => Mutools.Match.Is_Valid_Reference'Access);
   begin
      Mulog.Log (Msg => "Checking write access to crash audit region");

      if DOM.Core.Nodes.Length (List => Pairs.Left) > 0 then
         declare
            Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Pairs.Left,
                 Index => 0);
            Mem_Logical : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Mem_Node,
                 Name => "logical");
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Mem_Node,
                    Level => 2),
                 Name => "name");
         begin
            raise Validation_Error with "Logical memory node '" & Mem_Logical
              & "' of subject '" & Subj_Name & "' declares illegal write "
              & "access to crash audit region";
         end;
      end if;
   end Crash_Audit_Write_Access;

   -------------------------------------------------------------------------

   procedure Device_Mmconf_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Mmconf_Base : constant
        := Mutools.Constants.Subject_PCI_Config_Space_Addr;
      Devices_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/hardware/devices");
      Dev_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device/memory");
   begin
      Mulog.Log (Msg => "Checking subject device mmconf mappings");

      for I in 0 .. DOM.Core.Nodes.Length (List => Dev_Mem) - 1 loop
         declare
            Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Dev_Mem,
                 Index => I);
            Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Mem,
                 Name => "name");
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Mem),
                 Name => "name");
         begin
            if Mutools.XML_Utils.Is_Physical_Mmconf_Region
              (Devices_Node => Devices_Node,
               Addr         => Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute (Elem => Mem,
                                                   Name => "physicalAddress")))
            then
               declare
                  Mappings : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => XML_Data.Doc,
                       XPath => "/system/subjects/subject/devices/device"
                       & "[@physical='" & Dev_Name & "']/"
                       & "memory[@physical='" & Mem_Name & "']");
               begin
                  for J in 0 .. DOM.Core.Nodes.Length (List => Mappings) - 1
                  loop
                     declare
                        Mapping : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item
                            (List  => Mappings,
                             Index => J);
                        Dev_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => DOM.Core.Nodes.Parent_Node
                               (N => Mapping),
                             Name => "logical");
                        Subj_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Muxml.Utils.Ancestor_Node
                               (Node  => Mapping,
                                Level => 3),
                             Name => "name");
                        Addr : constant Interfaces.Unsigned_64
                          := Interfaces.Unsigned_64'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => Mapping,
                                Name => "virtualAddress"));
                        PCI : constant DOM.Core.Node
                          := Muxml.Utils.Get_Element
                            (Doc   => DOM.Core.Nodes.Parent_Node
                               (N => Mapping),
                             XPath => "pci");
                        Cfg_Addr : constant Interfaces.Unsigned_64
                          := Mutools.XML_Utils.Calculate_PCI_Cfg_Address
                            (Base_Address => Mmconf_Base,
                             PCI_Node     => PCI);
                     begin
                        if Cfg_Addr /= Addr then
                           raise Validation_Error with "PCI mmconf region of "
                             & "subject '" & Subj_Name & "' logical device '"
                             & Dev_Name & "' is "
                             & Mutools.Utils.To_Hex (Number => Addr)
                             & " but should be "
                             & Mutools.Utils.To_Hex (Number => Cfg_Addr);
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Device_Mmconf_Mappings;

   -------------------------------------------------------------------------

   procedure Global_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects   : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
      Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      --  Check that global subject IDs of Left and Right differ.
      procedure Check_Global_ID_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Global_ID_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_ID    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "globalId"));
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_ID   : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "globalId"));
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_ID = Right_ID then
            raise Validation_Error with "Subjects '" & Left_Name & "' and '"
              & Right_Name & "' have identical global ID" & Left_ID'Img;
         end if;
      end Check_Global_ID_Inequality;
   begin
      if Subj_Count > 1 then
         Mulog.Log (Msg => "Checking uniqueness of" & Subj_Count'Img
                    & " global subject ID(s)");
         Compare_All (Nodes      => Subjects,
                      Comparator => Check_Global_ID_Inequality'Access);
      end if;
   end Global_ID_Uniqueness;

   -------------------------------------------------------------------------

   procedure Initramfs_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns True if the left and right memory regions are adjacent.
      function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean;

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
      begin
         return "Initramfs region '" & Name & "' not adjacent to other "
           & "initramfs regions";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean
      is
      begin
         return Utils.Is_Adjacent_Region
           (Left      => Left,
            Right     => Right,
            Addr_Attr => "virtualAddress");
      end Is_Adjacent_Region;

      Phys_Memregions : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_initrd']");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Pairs : Muxml.Utils.Matching_Pairs_Type;
         begin
            Pairs := Muxml.Utils.Get_Matching
              (Left_Nodes     => Subj_Memory,
               Right_Nodes    => Phys_Memregions,
               Match_Multiple => True,
               Match          => Mutools.Match.Is_Valid_Reference'Access);

            if DOM.Core.Nodes.Length (List => Pairs.Left) > 1 then
               Mutools.XML_Utils.Set_Memory_Size
                 (Virtual_Mem_Nodes => Pairs.Left,
                  Ref_Nodes         => Phys_Memregions);
               For_Each_Match
                 (Source_Nodes => Pairs.Left,
                  Ref_Nodes    => Pairs.Left,
                  Log_Message  => "initramfs regions of subject '" & Subj_Name
                  & "' for consecutiveness",
                  Error        => Error_Msg'Access,
                  Match        => Is_Adjacent_Region'Access);
            end if;
         end;
      end loop;
   end Initramfs_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Local_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects  : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
      CPU_Count : constant Natural
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);

      Cur_CPU : Natural := 0;

      --  Check that local subject IDs of Left and Right differ.
      procedure Check_Local_ID_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Local_ID_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_ID    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "localId"));
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_ID   : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "localId"));
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_ID = Right_ID then
            raise Validation_Error with "Subjects '" & Left_Name & "' and '"
              & Right_Name & "' running on CPU" & Cur_CPU'Img
              & " have identical local ID" & Left_ID'Img;
         end if;
      end Check_Local_ID_Inequality;
   begin
      for I in Natural range 0 .. CPU_Count - 1 loop
         declare
            CPU_Str    : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            CPU_Subjs  : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Subjects,
                 Ref_Attr  => "cpu",
                 Ref_Value => CPU_Str);
            Subj_Count : constant Natural
              := DOM.Core.Nodes.Length (List => CPU_Subjs);
         begin
            if Subj_Count > 1 then
               Mulog.Log (Msg => "Checking" & Subj_Count'Img
                          & " local subject ID(s) of CPU " & CPU_Str
                          & " for uniqueness");
               Cur_CPU := I;
               Compare_All (Nodes      => CPU_Subjs,
                            Comparator => Check_Local_ID_Inequality'Access);
            end if;
         end;
      end loop;
   end Local_ID_Uniqueness;

   -------------------------------------------------------------------------

   procedure Logical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
      Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      --  Check that logical device names of Left and Right differ.
      procedure Check_Logical_Name_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Logical_Name_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Logical : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "logical");
         Right_Logical : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "logical");
      begin
         if Left_Logical = Right_Logical then
            raise Validation_Error with "devices with identical logical names"
              & " '" & Left_Logical & "'";
         end if;
      end Check_Logical_Name_Inequality;
   begin
      for I in Natural range 0 .. Subj_Count - 1 loop
         declare
            Cur_Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Subj,
               Name => "name");
            Devices  : constant DOM.Core.Node_List
              := XPath_Query (N     => Cur_Subj,
                              XPath => "devices/device");
            Dev_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Devices);
         begin
            if Dev_Count > 1 then
               Mulog.Log (Msg => "Checking" & Dev_Count'Img
                          & " logical devices of subject '" & Subj_Name
                          & "' for uniqueness");

               Compare_All
                 (Nodes      => Devices,
                  Comparator => Check_Logical_Name_Inequality'Access);
            end if;

         exception
            when E : Validation_Error =>
               raise Validation_Error with "Subject '" & Subj_Name & "' has "
                 & Ada.Exceptions.Exception_Message (X => E);
         end;
      end loop;
   end Logical_Device_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Logical_IRQ_MSI_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_MSI_Devs : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device"
           & "[pci/@msi='true' and count(irq) > 1]");
      Log_Devs : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/device"
           & "[pci and count(irq) > 1]");

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the left and right vectors are adjacent.
      function Is_Adjacent_Vector (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 3),
            Name => "name");
         Dev_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "logical");
         IRQ_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
      begin
         return "MSI IRQ '" & IRQ_Name & "' of logical device '" & Dev_Name
           & "' of subject '" & Subj_Name & "' not adjacent to other IRQs";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Is_Adjacent_Vector (Left, Right : DOM.Core.Node) return Boolean
      is
      begin
         return Utils.Is_Adjacent_Number
           (Left  => Left,
            Right => Right,
            Attr  => "vector");
      end Is_Adjacent_Vector;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Log_Devs) - 1 loop
         declare
            use type DOM.Core.Node;

            Log_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Log_Devs,
                 Index => I);
            Log_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Dev,
                 Name => "logical");
            Log_IRQs : constant DOM.Core.Node_List
              := XPath_Query (N     => Log_Dev,
                              XPath => "irq");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Dev,
                 Name => "physical");
            Phys_MSI_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_MSI_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
            Is_PCI_MSI_Device : constant Boolean := Phys_MSI_Dev /= null;
         begin
            if Is_PCI_MSI_Device then
               For_Each_Match
                 (Source_Nodes => Log_IRQs,
                  Ref_Nodes    => Log_IRQs,
                  Log_Message  => "PCI MSI IRQs of logical device '" & Log_Name
                  & "' for consecutiveness",
                  Error        => Error_Msg'Access,
                  Match        => Is_Adjacent_Vector'Access);
            end if;
         end;
      end loop;
   end Logical_IRQ_MSI_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Logical_Unmask_Event (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Devs : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device"
           & "[pci/@msi='false' and irq]");
      Subjs : constant DOM.Core.Node_List := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject[events/source/group/"
           & "event/unmask_irq]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjs) - 1 loop
         declare
            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjs,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Subj,
                                                  Name => "name");
            Ev_Actions : constant DOM.Core.Node_List := XPath_Query
              (N     => Subj,
               XPath => "events/source/group/event/unmask_irq");
            Log_Irqs : constant DOM.Core.Node_List := XPath_Query
              (N     => Subj,
               XPath => "devices/device/irq");
         begin
            Mulog.Log (Msg => "Checking 'Unmask_Irq' event logical names of "
                       & "subject '" & Subj_Name & "'");
            for J in 0 .. DOM.Core.Nodes.Length (List => Ev_Actions) - 1 loop
               declare
                  use type DOM.Core.Node;

                  Ev_Action : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Ev_Actions,
                                            Index => J);
                  Event : constant DOM.Core.Node
                    := DOM.Core.Nodes.Parent_Node (N => Ev_Action);
                  Ev_Log_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Event,
                       Name => "logical");
                  Delim_Idx : constant Natural
                    := Ada.Strings.Fixed.Index
                      (Source  => Ev_Log_Name,
                       Pattern => "_",
                       From    => Ev_Log_Name'Last,
                       Going   => Ada.Strings.Backward);
                  Vector_Nr : constant String
                    := Ev_Log_Name (Delim_Idx + 1 .. Ev_Log_Name'Last);
                  Ev_IRQ_Nr : constant String
                    := DOM.Core.Elements.Get_Attribute (Elem => Ev_Action,
                                                        Name => "number");

                  Log_IRQ : DOM.Core.Node;

                  --  Function returns the expected vector number for the given
                  --  physical IRQ number by resolving the corresponding
                  --  logical device IRQ vector. An empty string is returned if
                  --  the number could not be determined.
                  function Get_Expected_Vector
                    (IRQ_Number : String)
                     return String;

                  ----------------------------------------------------------

                  function Get_Expected_Vector
                    (IRQ_Number : String)
                     return String
                  is
                     Phys_Dev_Irqs : constant DOM.Core.Node_List
                       := XPath_Query
                         (N     => XML_Data.Doc,
                          XPath => "/system/hardware/devices/device"
                          & "/irq[@number='" & IRQ_Number & "']");
                  begin
                     for I in 0 .. DOM.Core.Nodes.Length
                       (List => Phys_Dev_Irqs) - 1
                     loop
                        declare
                           Phys_IRQ : constant DOM.Core.Node
                             := DOM.Core.Nodes.Item
                               (List  => Phys_Dev_Irqs,
                                Index => I);
                           Phys_Dev_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => DOM.Core.Nodes.Parent_Node
                                  (N => Phys_IRQ),
                                Name => "name");
                           Phys_IRQ_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Phys_IRQ,
                                Name => "name");
                           Log_IRQ : constant DOM.Core.Node
                             := Muxml.Utils.Get_Element
                               (Nodes     => Log_Irqs,
                                Ref_Attr  => "physical",
                                Ref_Value => Phys_IRQ_Name);
                           Log_Dev_Name : constant String
                             := (if Log_IRQ = null then ""
                                 else DOM.Core.Elements.Get_Attribute
                                   (Elem => DOM.Core.Nodes.Parent_Node
                                        (N => Log_IRQ),
                                    Name => "physical"));
                        begin
                           if Phys_Dev_Name = Log_Dev_Name then
                              return DOM.Core.Elements.Get_Attribute
                                (Elem => Log_IRQ,
                                 Name => "vector");
                           end if;
                        end;
                     end loop;
                     return "";
                  end Get_Expected_Vector;
               begin
                  if Ev_Log_Name (Ev_Log_Name'First .. Delim_Idx)
                    /= "unmask_irq_"
                  then
                     raise Validation_Error with "Logical event '"
                       & Ev_Log_Name & "' of subject '" & Subj_Name & "' "
                       & "has unexpected logical name: must have the form "
                       & "'unmask_irq_$VECTORNR'";
                  end if;

                  declare
                     Unused_Vector : Natural;
                  begin
                     Unused_Vector := Natural'Value (Vector_Nr);

                  exception
                     when Constraint_Error =>
                        raise Validation_Error with "Logical event '"
                          & Ev_Log_Name & "' of subject '" & Subj_Name
                          & "' has invalid suffix '" & Vector_Nr & "': must "
                          & "match number of corresponding logical IRQ vector";
                  end;

                  Log_IRQ := Muxml.Utils.Get_Element
                    (Nodes     => Log_Irqs,
                     Ref_Attr  => "vector",
                     Ref_Value => Vector_Nr);
                  if Log_IRQ = null then
                     declare
                        Exp_Vec : constant String
                          := Get_Expected_Vector (IRQ_Number => Ev_IRQ_Nr);
                     begin
                        raise Validation_Error with "Logical event '"
                          & Ev_Log_Name & "' of subject '" & Subj_Name & "' "
                          & "references invalid logical IRQ with vector "
                          & Vector_Nr & " as logical name suffix"
                          & (if Exp_Vec'Length = 0 then ""
                             else ": expected " & Exp_Vec);
                     end;
                  end if;

                  declare
                     Log_IRQ_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_IRQ,
                          Name => "physical");
                     Phys_IRQ_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_IRQ,
                          Name => "physical");
                     Log_Dev : constant DOM.Core.Node
                       := DOM.Core.Nodes.Parent_Node (N => Log_IRQ);
                     Log_Dev_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev,
                          Name => "logical");
                     Phys_Dev_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev,
                          Name => "physical");
                     Phys_Dev : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element (Nodes     => Phys_Devs,
                                                   Ref_Attr  => "name",
                                                   Ref_Value => Phys_Dev_Name);
                     Phys_IRQ : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Doc   => Phys_Dev,
                          XPath => "irq[@name='" & Phys_IRQ_Name & "']");
                     Phys_IRQ_Nr : constant String
                       := DOM.Core.Elements.Get_Attribute
                       (Elem => Phys_IRQ,
                        Name => "number");
                  begin
                     if Ev_IRQ_Nr /= Phys_IRQ_Nr then
                        raise Validation_Error with "Logical event '"
                          & Ev_Log_Name & "' of subject '" & Subj_Name & "' "
                          & "referencing logical IRQ " & Log_Dev_Name & "->"
                          & Log_IRQ_Name & " has unmask action number "
                          & "different from physical IRQ " & Phys_Dev_Name
                          & "->" & Phys_IRQ_Name & ": " & Ev_IRQ_Nr
                          & ", expected " & Phys_IRQ_Nr;
                     end if;
                  end;
               end;
            end loop;
         end;
      end loop;
   end Logical_Unmask_Event;

   -------------------------------------------------------------------------

   procedure Memory_Types (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Memory    : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Subject_Memory : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']/memory/memory");
   begin
      Mulog.Log (Msg => "Checking memory types of" & DOM.Core.Nodes.Length
                 (List => Subject_Memory)'Img & " subject memory mapping(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Subject_Memory) - 1 loop
         declare
            Virt_Mem     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subject_Memory,
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
         Left_ID    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "globalId");
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_ID   : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "globalId");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Subjects with global ID " & Left_ID
              & " and " & Right_ID & " have identical name '"
              & Left_Name & "'";
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
      Nodes : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => XML_Data,
           Left_XPath  => "/system/subjects/subject/devices/device",
           Right_XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference'Access);
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes.Left);
      Names : Unbounded_String;
   begin
      if Count > 0 then
         for I in 0 .. Count - 1 loop
            declare
               Subj      : constant DOM.Core.Node
                 := Muxml.Utils.Ancestor_Node
                   (Node  => DOM.Core.Nodes.Item
                      (List  => Nodes.Left,
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

            if Mutools.XML_Utils.Get_Executing_CPU
              (Data    => XML_Data,
               Subject => Subject) = -1
            then
               raise Validation_Error with "Subject '" & Subj_Name & "' is "
                 & "neither referenced in the scheduling plan nor "
                 & "schedulable via switch events";
            end if;
         end;
      end loop;
   end Runnability;

   -------------------------------------------------------------------------

   procedure Shared_Device_Same_PCI_Element (XML_Data : Muxml.XML_Data_Type)
   is
      Logical_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/device[pci]");
      Physical_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[pci]");

      --  Check that PCI element attributes match.
      procedure Check_PCI_Attrs (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_PCI_Attrs (Left, Right : DOM.Core.Node)
      is
         Left_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "logical");
         Left_PCI : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Left,
              XPath => "pci");
         Left_Bus : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left_PCI,
              Name => "bus");
         Left_Device : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left_PCI,
              Name => "device");
         Left_Func : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left_PCI,
              Name => "function");
         Right_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "logical");
         Right_PCI : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Right,
              XPath => "pci");
         Right_Bus : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right_PCI,
              Name => "bus");
         Right_Device : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right_PCI,
              Name => "device");
         Right_Func : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right_PCI,
              Name => "function");
      begin
         if Left_Bus /= Right_Bus
           or else Left_Device /= Right_Device
           or else Left_Func /= Right_Func
         then
            raise Validation_Error with "Shared logical devices '" & Left_Name
              & "|" & Right_Name & "' specify different PCI elements";
         end if;
      end Check_PCI_Attrs;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Physical_Devs) - 1 loop
         declare
            Phys_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Physical_Devs,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Dev,
                 Name => "name");
            Refs : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Logical_Devs,
                 Ref_Attr  => "physical",
                 Ref_Value => Dev_Name);
         begin
            if DOM.Core.Nodes.Length (List => Refs) > 1 then
               Mulog.Log (Msg => "Checking logical PCI elements of shared '"
                          & Dev_Name & "' device");
               Compare_All (Nodes      => Refs,
                            Comparator => Check_PCI_Attrs'Access);
            end if;
         end;
      end loop;
   end Shared_Device_Same_PCI_Element;

   -------------------------------------------------------------------------

   procedure Virtual_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Physical_Mem  : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[not(starts-with(@type,'system'))]");
      Physical_Devs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/devices/device");
      Subjects      : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            Memory        : constant DOM.Core.Node_List := XPath_Query
              (N     => Subject,
               XPath => "memory/memory");
            Dev_Memory    : constant DOM.Core.Node_List := XPath_Query
              (N     => Subject,
               XPath => "devices/device/memory");
            Dev_Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Dev_Memory);
            Mem_Nodes     : DOM.Core.Node_List;
         begin
            if DOM.Core.Nodes.Length (List => Memory) + Dev_Mem_Count > 1 then
               Mulog.Log (Msg => "Checking virtual memory overlap of subject '"
                          & Subj_Name & "'");

               for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
                  declare
                     Cur_Node : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                              (List  => Memory,
                               Index => J),
                          Deep => False);
                  begin
                     Mutools.XML_Utils.Set_Memory_Size
                       (Virtual_Mem_Node => Cur_Node,
                        Ref_Nodes        => Physical_Mem);
                     DOM.Core.Append_Node (List => Mem_Nodes,
                                           N    => Cur_Node);
                  end;
               end loop;

               for K in 0 .. Dev_Mem_Count - 1 loop
                  declare
                     Cur_Node      : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                       (List  => Dev_Memory,
                        Index => K);
                     Dev_Node      : constant DOM.Core.Node
                       := DOM.Core.Nodes.Parent_Node (N => Cur_Node);
                     Phys_Dev_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                       (Elem => Dev_Node,
                        Name => "physical");
                     Log_Dev_Name  : constant String
                       := DOM.Core.Elements.Get_Attribute
                       (Elem => Dev_Node,
                        Name => "logical");
                     Device        : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Physical_Devs,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Dev_Name);
                     Mem_Node      : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => Cur_Node,
                          Deep => False);
                     Mem_Name      : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "logical");
                  begin
                     Mutools.XML_Utils.Set_Memory_Size
                       (Virtual_Mem_Node => Mem_Node,
                        Ref_Nodes        => XPath_Query
                          (N     => Device,
                           XPath => "memory"));
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Mem_Node,
                        Name  => "logical",
                        Value => Log_Dev_Name & "->" & Mem_Name);
                     DOM.Core.Append_Node (List => Mem_Nodes,
                                           N    => Mem_Node);
                  end;
               end loop;

               Check_Memory_Overlap
                 (Nodes        => Mem_Nodes,
                  Region_Type  => "virtual memory region",
                  Address_Attr => "virtualAddress",
                  Name_Attr    => "logical",
                  Add_Msg      => " of subject '" & Subj_Name & "'");
            end if;
         end;
      end loop;
   end Virtual_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure VMX_Controls_Entry_Checks (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns True if the VMX control specified by XPath is set to 1.
      function Is_Set
        (Ctrls : DOM.Core.Node;
         XPath : String)
         return Boolean;

      ----------------------------------------------------------------------

      function Is_Set
        (Ctrls : DOM.Core.Node;
         XPath : String)
         return Boolean
      is
         Ctrl_Val_Str : constant String
           := Muxml.Utils.Get_Element_Value (Doc   => Ctrls,
                                             XPath => XPath);
      begin
         return Ctrl_Val_Str = "1";
      end Is_Set;

      ----------------------------------------------------------------------

      Phys_Mem : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");
      Subjects : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Subjects);
   begin
      for I in 0 .. Count - 1 loop
         declare
            Subject : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            VMX_Ctrls : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subject,
               XPath => "vcpu/vmx/controls");
         begin
            Mulog.Log (Msg => "Checking VMX controls of subject '" & Subj_Name
                       & "'");

            if Is_Set (Ctrls => VMX_Ctrls,
                       XPath => "proc/UseIOBitmaps")
            then
               declare
                  Bit_Mask : constant Interfaces.Unsigned_64
                    := 2#1111_1111_1111#;
                  IOBM_Addr : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (Muxml.Utils.Get_Attribute
                         (Nodes     => Phys_Mem,
                          Ref_Attr  => "name",
                          Ref_Value => Subj_Name & "|iobm",
                          Attr_Name => "physicalAddress"));
               begin
                  if (IOBM_Addr and Bit_Mask) /= 0 then
                     raise Validation_Error with "Address of I/O Bitmap of "
                       & "subject '" & Subj_Name & "' invalid: bits 11:0 must "
                       & "be zero";
                  end if;
               end;
            end if;

            if Is_Set (Ctrls => VMX_Ctrls,
                       XPath => "proc/UseMSRBitmaps")
            then
               declare
                  Bit_Mask : constant Interfaces.Unsigned_64
                    := 2#1111_1111_1111#;
                  MSRBM_Addr : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (Muxml.Utils.Get_Attribute
                         (Nodes     => Phys_Mem,
                          Ref_Attr  => "name",
                          Ref_Value => Subj_Name & "|msrbm",
                          Attr_Name => "physicalAddress"));
               begin
                  if (MSRBM_Addr and Bit_Mask) /= 0 then
                     raise Validation_Error with "Address of MSR Bitmap of "
                       & "subject '" & Subj_Name & "' invalid: bits 11:0 must "
                       & "be zero";
                  end if;
               end;
            end if;

            if not Is_Set (Ctrls => VMX_Ctrls,
                       XPath => "pin/NMIExiting")
              and Is_Set (Ctrls => VMX_Ctrls,
                          XPath => "pin/VirtualNMIs")
            then
               raise Validation_Error
                 with "VMX control 'NMI-Exiting' is 0 for subject '"
                 & Subj_Name & "' but 'Virtual NMIs' is 1";
            end if;
         end;
      end loop;
   end VMX_Controls_Entry_Checks;

end Mucfgcheck.Subject;
