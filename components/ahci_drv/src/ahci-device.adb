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

with Ata;

with Ahci.Commands;
with Ahci.Delays;
with Ahci.HBA;
with Ahci.Ports;

with Debug_Ops;
with Interfaces;

with SK.Strings;

package body Ahci.Device
is

   -------------------------------------------------------------------------

   procedure RW_Sectors
      (ID      :     Port_Range;
       RW      :     RW_Type;
       Start   :     Interfaces.Unsigned_64;
       Count   :     Interfaces.Unsigned_32;
       Address :     Interfaces.Unsigned_64;
       Ret_Val : out Status_Type)
   is
      Signature : constant Signature_Type := Devices (ID).Signature;
   begin
      case Signature is
         when Sata =>
            Ata.RW_Sectors (ID => ID,
               RW      => RW,
               Start   => Start,
               Count   => Count,
               Address => Address,
               Ret_Val => Ret_Val);
         when others =>
            Ret_Val := ENOTSUP;
      end case;
   end RW_Sectors;

   -------------------------------------------------------------------------

   procedure Discard_Sectors
      (ID      :     Port_Range;
       Start   :     Interfaces.Unsigned_64;
       Count   :     Interfaces.Unsigned_32;
       Ret_Val : out Status_Type)
   is
      Signature : constant Signature_Type := Devices (ID).Signature;
   begin
      case Signature is
         when Sata =>
            Ata.Discard_Sectors
               (ID    => ID,
                Start => Start,
                Count => Count,
                Ret_Val => Ret_Val);
         when others =>
            Ret_Val := ENOTSUP;
      end case;
   end Discard_Sectors;

   -------------------------------------------------------------------------

   procedure Get_Attached_Devices (Dev : out Bit_Array)
   is
   begin
      for I in Port_Range loop
         Dev (Integer (I)) := Devices (I).Signature /= Empty;
      end loop;
   end Get_Attached_Devices;

   -------------------------------------------------------------------------

   function Get_Size (ID : Port_Range) return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;
   begin
      return Interfaces.Unsigned_64 (Get_Sector_Size (ID)) *
               Get_Sector_Cnt (ID);
   end Get_Size;

   -------------------------------------------------------------------------

   function Get_Sector_Size (ID : Port_Range) return Interfaces.Unsigned_32
   is (Devices (ID).Sector_Size);

   -------------------------------------------------------------------------

   function Get_Sector_Cnt (ID : Port_Range) return Interfaces.Unsigned_64
   is (Devices (ID).Number_Of_Sectors);

   -------------------------------------------------------------------------

   procedure Identify_Device
      (Port_ID : Port_Range)
   is
      Sig : Interfaces.Unsigned_32;
   begin
      Sig := Ports.Instance (Port_ID).Signature;
      case Sig is
         when Ports.SIG_ATA =>
            Ata.Identify_Device (Port_ID => Port_ID);
            Devices (Port_ID).Signature := Sata;
         when Ports.SIG_ATAPI =>
            Devices (Port_ID).Signature := Atapi;
         when others =>
            Devices (Port_ID).Signature := Empty;
      end case;

   end Identify_Device;

   -------------------------------------------------------------------------

   procedure Probe
      (Port_ID : Port_Range)
   is
      use type Interfaces.Unsigned_64;

      HBA_Caps       : constant HBA.HBA_Caps_Type
                         := HBA.Instance.Host_Capabilities;
      Success        : Boolean;
      Address        : Interfaces.Unsigned_64;
      Timeout        : Natural := 3000;
      Clear_Err      : Ports.Clear_Error_Type;

      Cmd_List_Size  : constant := 16#400#;
      Cmd_Table_Size : constant := 16#100#;
      Fis_Size       : constant := 16#100#;

      function Upper (I : Interfaces.Unsigned_64) return Interfaces.Unsigned_32
      is (Interfaces.Unsigned_32 (Interfaces.Shift_Right (I, 32)));

      function Lower (I : Interfaces.Unsigned_64) return Interfaces.Unsigned_32
      is (Interfaces.Unsigned_32 (I));
   begin

      if HBA_Caps.SSS then
         pragma Debug (Debug_Ops.Put_Line ("HBA supports staged spin up"));
         Ports.Spin_Up (ID => Port_ID);
      end if;

      --  wait 1s if it's the first port
      if Port_ID = 0 then
         for I in Natural range 1 .. 10 loop
            Ports.Is_Active (ID => Port_ID, Active => Success);
            exit when Success;
            Delays.M_Delay (Msec => 100);
         end loop;
      else
         Ports.Is_Active (ID => Port_ID, Active => Success);
      end if;

      if not Success then
         return;
      end if;

      Clear_Err.Sata := True;
      Clear_Err.Intr := True;
      Ports.Clear_Errors (ID => Port_ID, Clear => Clear_Err);

      Ports.Stop (ID => Port_ID);

      Address := Command_Lists_Address
                     + Interfaces.Unsigned_64 (Port_ID) * Cmd_List_Size;
      Ports.Instance (Port_ID).Cmd_List_Base_Addr := Lower (Address);
      Ports.Instance (Port_ID).Cmd_List_Base_Upper_Addr := Upper (Address);

      Address := Fis_Base_Address
                  + Interfaces.Unsigned_64 (Port_ID) * Fis_Size;
      Ports.Instance (Port_ID).FIS_Base_Addr := Lower (Address);
      Ports.Instance (Port_ID).FIS_Base_Upper_Addr := Upper (Address);

      --  setup command table address in command header. For now it's
      --  a fixed mapping between Cmd[0] and Command_Header[0].
      Address := Command_Table_Address
         + Interfaces.Unsigned_64 (Port_ID) * Cmd_Table_Size;
      Commands.Command_Lists (Port_ID)(0).CTBA
         := Unsigned_25 (Interfaces.Shift_Right
               (Interfaces.Unsigned_32 (Address), 7));
      Commands.Command_Lists (Port_ID)(0).CTBAU := Upper (Address);

      Ports.Start (ID => Port_ID); -- start cmd engine + fre
      Ports.Enable (ID => Port_ID, Success => Success);
      if not Success then
         return;
      end if;

      --  Wait for D2H Register FIS with device' signature.
      --  The drive has to spin up here, so wait up to 30s
      Wait_Device_Signature : loop
         Success := Ports.Instance (Port_ID).Task_File_Data.STS.BSY;
         exit Wait_Device_Signature when
            (Success = False) or (Timeout = 0);
         Timeout := Timeout - 1;
         Delays.M_Delay (Msec => 10);
      end loop Wait_Device_Signature;

      Identify_Device (Port_ID => Port_ID);

      pragma Debug (Devices (Port_ID).Signature = Sata,
         Debug_Ops.Put_Line ("Sata device found on Port " &
         SK.Strings.Img (Interfaces.Unsigned_8 (Port_ID))));
   end Probe;

   -------------------------------------------------------------------------

   procedure Init
   is
      PI : constant Bit_Array := HBA.Instance.Ports_Implemented;
   begin
      for I in Port_Range loop
         if PI (Natural (I)) then
            Probe (Port_ID => I);
         end if;
      end loop;
   end Init;

end Ahci.Device;
