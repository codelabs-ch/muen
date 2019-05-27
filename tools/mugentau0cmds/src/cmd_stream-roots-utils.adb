--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mutools.Constants;
with Mutools.Utils;

with Paging.Layouts;
with Paging.Tables;

with Cmd_Stream.Constants;

package body Cmd_Stream.Roots.Utils
is

   package MC renames Mutools.Constants;

   --  Assign device memory to given object.
   procedure Assign_Device_Memory
     (Stream_Doc     :        Muxml.XML_Data_Type;
      Map_Cmd_Buffer : in out XML_Utils.Command_Buffer_Type;
      Mem_Layout     : in out Paging.Layouts.Memory_Layout_Type;
      Physical_Devs  :        DOM.Core.Node_List;
      Logical_Devs   :        DOM.Core.Node_List;
      Object_Attr    :        Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind    :        String);

   --  Generate object page table commands and return activation commands in
   --  given command buffer.
   procedure Create_Object_PTs
     (Stream_Doc    :     Muxml.XML_Data_Type;
      Activate_Cmds : out XML_Utils.Command_Buffer_Type;
      Object_Attr   :     Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind   :     String;
      Mem_Layout    :     Paging.Layouts.Memory_Layout_Type;
      PT_Address    :     Interfaces.Unsigned_64;
      PT_Size       :     Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Assign_Device_Memory
     (Stream_Doc     :        Muxml.XML_Data_Type;
      Map_Cmd_Buffer : in out XML_Utils.Command_Buffer_Type;
      Mem_Layout     : in out Paging.Layouts.Memory_Layout_Type;
      Physical_Devs  :        DOM.Core.Node_List;
      Logical_Devs   :        DOM.Core.Node_List;
      Object_Attr    :        Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind    :        String)
   is
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Devs) - 1 loop
         declare
            Log_Dev : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Logical_Devs,
               Index => I);
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Dev,
                 Name => "physical");
            Phys_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
            Phys_Dev_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Phys_Dev,
                 XPath => "memory");
            Log_Dev_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Log_Dev,
                 XPath => "memory");
            Dev_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("device"),
                  Value => U (DOM.Core.Elements.Get_Attribute
                    (Elem => Phys_Dev,
                     Name => "tau0DeviceId")));
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Log_Dev_Memory) - 1
            loop
               declare
                  use type Interfaces.Unsigned_64;

                  Log_Dev_Mem : constant  DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Log_Dev_Memory,
                       Index => J);
                  Virt_Addr : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Mem,
                          Name => "virtualAddress"));
                  Writable : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Log_Dev_Mem,
                       Name => "writable");
                  Executable : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Log_Dev_Mem,
                       Name => "executable");
                  Phys_Dev_Mem_Name : constant  String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Log_Dev_Mem,
                       Name => "physical");
                  Phys_Dev_Mem : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Dev_Memory,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Dev_Mem_Name);
                  Phys_Addr : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Dev_Mem,
                          Name => "physicalAddress"));
                  Size : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Dev_Mem,
                          Name => "size"));
                  Caching : constant Paging.Caching_Type
                    := Paging.Caching_Type'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Dev_Mem,
                          Name => "caching"));

                  End_Virt_Addr : constant Interfaces.Unsigned_64
                    := Virt_Addr + Size;
                  Cur_Offset : Interfaces.Unsigned_64 := 0;
               begin
                  while Virt_Addr + Cur_Offset < End_Virt_Addr loop
                     XML_Utils.Append_Command
                       (Stream_Doc => Stream_Doc,
                        Buffer     => Map_Cmd_Buffer,
                        Name       => "mapDevicePage" & Object_Kind,
                        Attrs      => (Object_Attr,
                                       Dev_Attr,
                                       (Attr  => U ("virtualAddress"),
                                        Value => U
                                          (Mutools.Utils.To_Hex
                                             (Number => Virt_Addr
                                              + Cur_Offset))),
                                       (Attr  => U ("page"),
                                        Value => U (Mutools.Utils.To_Hex
                                          (Number => Phys_Addr
                                           + Cur_Offset))),
                                       (Attr  => U ("writable"),
                                        Value => U (Writable)),
                                       (Attr  => U ("executable"),
                                        Value => U (Executable))));
                     Cur_Offset := Cur_Offset + MC.Page_Size;
                  end loop;

                  Paging.Layouts.Add_Memory_Region
                    (Mem_Layout       => Mem_Layout,
                     Physical_Address => Phys_Addr,
                     Virtual_Address  => Virt_Addr,
                     Size             => Size,
                     Caching          => Caching,
                     Writable         => Boolean'Value (Writable),
                     Executable       => Boolean'Value (Executable));
               end;
            end loop;
         end;
      end loop;
   end Assign_Device_Memory;

   -------------------------------------------------------------------------

   procedure Assign_Memory
     (Stream_Doc    : Muxml.XML_Data_Type;
      Physical_Mem  : DOM.Core.Node_List;
      Physical_Devs : DOM.Core.Node_List;
      Logical_Mem   : DOM.Core.Node_List;
      Logical_Devs  : DOM.Core.Node_List;
      Object_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind   : String;
      Entity_Name   : String)
   is
      PT_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
             (Nodes     => Physical_Mem,
              Ref_Attr  => "name",
              Ref_Value => Entity_Name);
      PT_Addr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PT_Node,
              Name => "physicalAddress"));
      PT_Size : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PT_Node,
              Name => "size"));
      Mem_Layout : Paging.Layouts.Memory_Layout_Type (Levels => 4);
      Map_Cmd_Buf, Activate_PT_Buf : XML_Utils.Command_Buffer_Type;
   begin
      Paging.Layouts.Set_Address
        (Mem_Layout => Mem_Layout,
         Address    => PT_Addr);
      Paging.Layouts.Set_Large_Page_Support
        (Mem_Layout => Mem_Layout,
         State      => False);
      for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Mem) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Virt_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Logical_Mem,
                                      Index => I);
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Virt_Mem,
                    Name => "virtualAddress"));
            Writable : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "writable");
            Executable : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "executable");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "physical");
            Phys_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Mem,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
            Phys_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "physicalAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "size"));
            Caching : constant Paging.Caching_Type
              := Paging.Caching_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "caching"));
            MR_ID_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Mem,
                 Name => Constants.MR_ID_Attr_Name);
            Region_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("region"),
                  Value => U (MR_ID_Str));
            Cur_Table_Idx : constant Natural := Allocate_Page_Table;
            Table_Idx_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("tableIndex"),
                  Value => U (Trim (Cur_Table_Idx'Img)));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "attachMemoryRegion" & Object_Kind,
               Attrs      => (Object_Attr,
                              Region_Attr,
                              (Attr  => U ("offset"),
                               Value => U ("0")),
                              (Attr  => U ("length"),
                               Value => U (Trim
                                 (Interfaces.Unsigned_64'Image
                                    (Size / MC.Page_Size)))),
                              Table_Idx_Attr,
                              (Attr  => U ("writable"),
                               Value => U (Writable)),
                              (Attr  => U ("executable"),
                               Value => U (Executable))));

            declare
               Page_Count : constant Interfaces.Unsigned_64
                 := Size / MC.Page_Size;
            begin
               XML_Utils.Append_Command
                 (Buffer     => Map_Cmd_Buf,
                  Stream_Doc => Stream_Doc,
                  Name       => "mapPages" & Object_Kind,
                  Attrs      => (Object_Attr,
                                 Table_Idx_Attr,
                                 (Attr  => U ("baseVirtualAddress"),
                                  Value => U (Mutools.Utils.To_Hex
                                    (Number => Virt_Addr))),
                                 (Attr  => U ("baseOffset"),
                                  Value => U ("0")),
                                 (Attr  => U ("count"),
                                  Value => U
                                    (Trim (Interfaces.Unsigned_64'Image
                                     (Page_Count))))));
            end;

            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Mem_Layout,
               Physical_Address => Phys_Addr,
               Virtual_Address  => Virt_Addr,
               Size             => Size,
               Caching          => Caching,
               Writable         => Boolean'Value (Writable),
               Executable       => Boolean'Value (Executable));
         end;
      end loop;

      Assign_Device_Memory
        (Stream_Doc     => Stream_Doc,
         Map_Cmd_Buffer => Map_Cmd_Buf,
         Mem_Layout     => Mem_Layout,
         Physical_Devs  => Physical_Devs,
         Logical_Devs   => Logical_Devs,
         Object_Attr    => Object_Attr,
         Object_Kind => Object_Kind);

      Create_Object_PTs
        (Stream_Doc    => Stream_Doc,
         Activate_Cmds => Activate_PT_Buf,
         Object_Attr   => Object_Attr,
         Object_Kind   => Object_Kind,
         Mem_Layout    => Mem_Layout,
         PT_Address    => PT_Addr,
         PT_Size       => PT_Size);

      XML_Utils.Append_Commands
        (Stream_Doc => Stream_Doc,
         Buffer     => Map_Cmd_Buf);

      XML_Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "lock" & Object_Kind,
         Attrs      => (1 => Object_Attr));

      XML_Utils.Append_Commands
        (Stream_Doc => Stream_Doc,
         Buffer     => Activate_PT_Buf);
   end Assign_Memory;

   -------------------------------------------------------------------------

   procedure Create_Object_PTs
     (Stream_Doc    :     Muxml.XML_Data_Type;
      Activate_Cmds : out XML_Utils.Command_Buffer_Type;
      Object_Attr   :     Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind   :     String;
      Mem_Layout    :     Paging.Layouts.Memory_Layout_Type;
      PT_Address    :     Interfaces.Unsigned_64;
      PT_Size       :     Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      Cur_PT_Addr : Interfaces.Unsigned_64 := PT_Address + PT_Size;

      --  Generate corresponding create object page table command for given
      --  table.
      procedure Process_Table
        (Level       : Paging.Paging_Level;
         Table_Index : Paging.Table_Range;
         Table       : Paging.Tables.Page_Table_Type);

      -------------------------------------------------------------------

      procedure Process_Table
        (Level       : Paging.Paging_Level;
         Table_Index : Paging.Table_Range;
         Table       : Paging.Tables.Page_Table_Type)
      is
         pragma Unreferenced (Table);

         Physical_Address : constant Interfaces.Unsigned_64
           := (if Level = 1 then PT_Address else Cur_PT_Addr);
         Phys_Add_Str : constant String
           := Mutools.Utils.To_Hex (Number => Physical_Address);
         Virtual_Address  : constant Interfaces.Unsigned_64
           := Paging.Get_Base_Address
             (Index => Table_Index,
              Level => Level);
      begin
         XML_Utils.Append_Command
           (Stream_Doc => Stream_Doc,
            Name       => "createPageTable" & Object_Kind,
            Attrs      => (Object_Attr,
                           (Attr  => U ("page"),
                            Value => U (Phys_Add_Str)),
                           (Attr  => U ("level"),
                            Value => U (Trim (Positive'Image (5 - Level)))),
                           (Attr  => U ("virtualAddress"),
                            Value => U (Mutools.Utils.To_Hex
                              (Number => Virtual_Address))),
                           (Attr  => U ("readable"),
                            Value => U ("true")),
                           (Attr  => U ("writable"),
                            Value => U ("true")),
                           (Attr  => U ("executable"),
                            Value => U ("true"))));

         XML_Utils.Append_Command
           (Buffer     => Activate_Cmds,
            Stream_Doc => Stream_Doc,
            Name       => "activatePageTable" & Object_Kind,
            Attrs      => (Object_Attr,
                           (Attr  => U ("level"),
                            Value => U (Trim (Positive'Image (5 - Level)))),
                           (Attr  => U ("virtualAddress"),
                            Value => U (Mutools.Utils.To_Hex
                              (Number => Virtual_Address)))));
         Cur_PT_Addr := Cur_PT_Addr - MC.Page_Size;
      end Process_Table;
   begin
      XML_Utils.Clear_Region
        (Stream_Doc   => Stream_Doc,
         Base_Address => PT_Address,
         Size         => PT_Size);
      Paging.Layouts.Traverse_Tables
        (Mem_Layout  => Mem_Layout,
         Process     => Process_Table'Access);

      XML_Utils.Reverse_Commands (Buffer => Activate_Cmds);
   end Create_Object_PTs;

end Cmd_Stream.Roots.Utils;
