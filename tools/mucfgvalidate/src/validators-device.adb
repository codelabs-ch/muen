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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;

package body Validators.Device
is

   use McKae.XML.XPath.XIA;

   --  Returns True if the device and resource reference names match.
   function Is_Valid_Resource_Ref (Left, Right : DOM.Core.Node) return Boolean;

   -------------------------------------------------------------------------

   procedure IO_Port_Range_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the device I/O port range matches.
      function Match_Port_Range (Left, Right : DOM.Core.Node) return Boolean;

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
         return "I/O port ranges of physical '" & Phys_Name & "' and logical"
           & " I/O port '" & Logical_Name & "' of logical device '"
           & Log_Dev_Name & "' differ";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Port_Range (Left, Right : DOM.Core.Node) return Boolean
      is
         use Interfaces;

         L_Start_Addr : constant Unsigned_32 := Unsigned_32'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "start"));
         L_End_Addr   : constant Unsigned_32 := Unsigned_32'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "end"));
         P_Start_Addr : constant Unsigned_32 := Unsigned_32'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "start"));
         P_End_Addr   : constant Unsigned_32 := Unsigned_32'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "end"));
      begin
         return L_Start_Addr = P_Start_Addr and then L_End_Addr = P_End_Addr
           and then Is_Valid_Resource_Ref (Left  => Left,
                                           Right => Right);
      end Match_Port_Range;
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//ioPort[@logical]",
                      Ref_XPath    => "/system/platform/device/ioPort",
                      Log_Message  => "I/O port range(s) for equality",
                      Error        => Error_Msg'Access,
                      Match        => Match_Port_Range'Access);
   end IO_Port_Range_Equality;

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
                      Ref_XPath    => "/system/platform/device/ioPort",
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
         XPath => "//ioPort");
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

   procedure IRQ_Number_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the device IRQ number matches.
      function Match_IRQ_Number (Left, Right : DOM.Core.Node) return Boolean;

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
         return "Physical IRQ '" & Phys_Name & "' and logical IRQ '"
           & Logical_Name & "' of logical device '" & Log_Dev_Name
           & "' differ";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_IRQ_Number (Left, Right : DOM.Core.Node) return Boolean
      is
         Log_IRQ_Str  : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "number");
         Logical_IRQ  : constant Natural := Natural'Value (Log_IRQ_Str);
         Phys_IRQ_Str : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "number");
         Physical_IRQ : constant Natural := Natural'Value (Phys_IRQ_Str);
      begin
         return Logical_IRQ = Physical_IRQ
           and then Is_Valid_Resource_Ref (Left  => Left,
                                           Right => Right);
      end Match_IRQ_Number;
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//irq[@logical]",
                      Ref_XPath    => "/system/platform/device/irq",
                      Log_Message  => "device IRQ(s) for equality",
                      Error        => Error_Msg'Access,
                      Match        => Match_IRQ_Number'Access);
   end IRQ_Number_Equality;

   -------------------------------------------------------------------------

   function Is_Valid_Resource_Ref (Left, Right : DOM.Core.Node) return Boolean
   is
   begin
      return Is_Valid_Reference (Left  => Left,
                                 Right => Right)
        and then Is_Valid_Reference
          (Left  => DOM.Core.Nodes.Parent_Node (N => Left),
           Right => DOM.Core.Nodes.Parent_Node (N => Right));
   end Is_Valid_Resource_Ref;

   -------------------------------------------------------------------------

   procedure Physical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/device");

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
                      Ref_XPath    => "/system/platform/device",
                      Log_Message  => "physical device reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Reference'Access);
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
                      Ref_XPath    => "/system/platform/device/irq",
                      Log_Message  => "device IRQ reference(s)",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Resource_Ref'Access);
   end Physical_IRQ_References;

   -------------------------------------------------------------------------

   procedure Physical_IRQ_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/device/irq");

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
                 (List => Nodes)'Img& " device IRQ(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Physical_IRQ_Uniqueness;

end Validators.Device;
