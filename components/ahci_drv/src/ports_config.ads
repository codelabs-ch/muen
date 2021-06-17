--
--  Copyright (C) 2020 secunet Security Networks AG
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

with Ahci;
with Ahci_Drv_Component.Channel_Arrays;
with Ahci_Drv_Component.Memory_Arrays;

package Ports_Config is
   --  Use No_Partition if you want to export the whole Device attached
   --  to the given AHCI_Port. Smart_Only will not assign a partition to the
   --  device and allow only the Get_SMART request
   No_Partition   : Integer := Natural'Last;
   Null_Partition : Integer := 16#cafe#;
   Smart_Only     : Integer := 16#beef#;

   --  Maximum zero based device index of devices attached to a server port
   Devices_Max : constant := 1;

   --  Number of used Server Ports
   Ports_Max : constant := 2;

   package CSpecs renames Ahci_Drv_Component.Channel_Arrays;
   package MSpecs renames Ahci_Drv_Component.Memory_Arrays;

   pragma Compile_Time_Error
      (((CSpecs.Blockdev_Request_Element_Count /=
            CSpecs.Blockdev_Response_Element_Count) or
        (CSpecs.Blockdev_Response_Element_Count /=
            MSpecs.Blockdev_Shm_Element_Count)),
         "Number of Request / Response and Shm regions must be equal.");

   type Channel_Range is range 0 .. CSpecs.Blockdev_Request_Element_Count - 1;

   --  Defines a exported device by describing it's Ahci_Port and the
   --  zero based partition Number
   type Device is record
      Ahci_Port : Ahci.Port_Range;
      Partition : Natural;
   end record;

   Null_Device : Device :=
      (Ahci_Port => 0,
      Partition => Null_Partition);

   type Devices_Range is range 0 .. Devices_Max;
   type Devices_Array_Type is array (Devices_Range) of Device;

   --  Defines a Server Port. Chan_Idx is used as offset in the
   --  Request/Response Channel Arrays
   type Port_Config_Type is record
      Chan_Idx : Channel_Range;
      Devices  : Devices_Array_Type;
   end record;

   type Ports_Array_Range is range 1 .. Ports_Max;
   type Port_Config_Array_Type is array (Ports_Array_Range)
      of Port_Config_Type;

   --  nuc: 0
   --  qemu: 1
   Device_ID : constant Ahci.Port_Range := 0;

   --  configuration of the server ports
   Port_Config : constant Port_Config_Array_Type :=
      (Port_Config_Type'(
         Chan_Idx => 0,
         Devices  => (
            Device'(Ahci_Port => Device_ID, Partition => No_Partition),
            others => Null_Device)),
       Port_Config_Type'(
          Chan_Idx => 1,
          Devices  => (
             Device'(Ahci_Port => Device_ID, Partition => 4),
             others => Null_Device))
      );

end Ports_Config;
