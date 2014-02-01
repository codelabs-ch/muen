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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Mulog;

with Validate;
with Validators.Memory;
with Validators.MSR;
with Validators.Device;
with Validators.Scheduling;
with Validators.Subject;
with Validators.Platform;

package body Validators
is

   -------------------------------------------------------------------------

   procedure Check_Attribute
     (Nodes     : DOM.Core.Node_List;
      Node_Type : String;
      Attr      : String;
      Name_Attr : String;
      Test      : Test_Function;
      Right     : Interfaces.Unsigned_64;
      Error_Msg : String)
   is
   begin
      Mulog.Log (Msg => "Checking attribute '" & Attr & "' of"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " "
                 & Node_Type & " element(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Name       : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Name_Attr);
            Attr_Str   : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Attr);
            Attr_Value : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Attr_Str);
         begin
            if not Test (Attr_Value, Right) then
               raise Validation_Error with "Attribute '" & Attr & " => "
                 & Attr_Str & "' of '" & Name & "' " & Node_Type  & " element "
                 & Error_Msg;
            end if;
         end;
      end loop;
   end Check_Attribute;

   -------------------------------------------------------------------------

   procedure Check_Memory_Overlap
     (Nodes        : DOM.Core.Node_List;
      Region_Type  : String;
      Address_Attr : String)
   is
      use Interfaces;
   begin
      Mulog.Log (Msg => "Checking overlap of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " " & Region_Type & "(s)");

      if DOM.Core.Nodes.Length (List => Nodes) < 2 then
         return;
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 2 loop
         declare
            Cur_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Cur_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Node,
                 Name => "name");
            Cur_Addr : constant Unsigned_64 := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Node,
                  Name => Address_Attr));
            Cur_Size : constant Unsigned_64 := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Node,
                  Name => "size"));
         begin
            for J in I + 1 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
               declare
                  Other_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
                    (List  => Nodes,
                     Index => J);
                  Other_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Other_Node,
                       Name => "name");
                  Other_Addr : constant Unsigned_64 := Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Other_Node,
                        Name => Address_Attr));
                  Other_Size : constant Unsigned_64 := Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Other_Node,
                        Name => "size"));
               begin
                  if (Cur_Addr <= Other_Addr
                      and then Cur_Addr + Cur_Size > Other_Addr)
                    or
                      (Other_Addr < Cur_Addr
                       and then Other_Addr + Other_Size > Cur_Addr)
                  then
                     raise Validation_Error with "Overlap of " & Region_Type
                       & " '" & Cur_Name & "' and '" & Other_Name & "'";
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Check_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Validate.Register
        (Validator => Memory.Physical_Memory_References'Access);
      Validate.Register
        (Validator => Memory.VMXON_Region_Presence'Access);
      Validate.Register
        (Validator => Memory.VMXON_Region_Size'Access);
      Validate.Register
        (Validator => Memory.VMXON_In_Lowmem'Access);
      Validate.Register
        (Validator => Memory.VMCS_Region_Presence'Access);
      Validate.Register
        (Validator => Memory.VMCS_Region_Size'Access);
      Validate.Register
        (Validator => Memory.VMCS_In_Lowmem'Access);
      Validate.Register
        (Validator => Memory.Physical_Address_Alignment'Access);
      Validate.Register
        (Validator => Memory.Virtual_Address_Alignment'Access);
      Validate.Register
        (Validator => Memory.Region_Size'Access);
      Validate.Register
        (Validator => Memory.Entity_Name_Encoding'Access);
      Validate.Register
        (Validator => Memory.Physical_Memory_Overlap'Access);
      Validate.Register
        (Validator => MSR.Start_Smaller_End'Access);
      Validate.Register
        (Validator => MSR.Low_Or_High'Access);
      Validate.Register
        (Validator => Device.Physical_Device_References'Access);
      Validate.Register
        (Validator => Device.Physical_IRQ_Uniqueness'Access);
      Validate.Register
        (Validator => Device.Physical_IRQ_References'Access);
      Validate.Register
        (Validator => Device.IRQ_Number_Equality'Access);
      Validate.Register
        (Validator => Device.IO_Port_Start_Smaller_End'Access);
      Validate.Register
        (Validator => Device.IO_Port_References'Access);
      Validate.Register
        (Validator => Device.IO_Port_Range_Equality'Access);
      Validate.Register
        (Validator => Scheduling.CPU_Element_Count'Access);
      Validate.Register
        (Validator => Scheduling.Subject_References'Access);
      Validate.Register
        (Validator => Scheduling.Subject_CPU_Affinity'Access);
      Validate.Register
        (Validator => Scheduling.Major_Frame_Ticks'Access);
      Validate.Register
        (Validator => Subject.Name_Uniqueness'Access);
      Validate.Register
        (Validator => Subject.CPU_ID'Access);
      Validate.Register
        (Validator => Subject.Event_Subject_References'Access);
      Validate.Register
        (Validator => Subject.Event_Self_References'Access);
      Validate.Register
        (Validator => Subject.Event_Switch_Same_Core'Access);
      Validate.Register
        (Validator => Subject.Event_IPI_Different_Core'Access);
      Validate.Register
        (Validator => Platform.Memory_Space'Access);
      Validate.Register
        (Validator => Platform.Memory_Block_Overlap'Access);
   end Register_All;

end Validators;
