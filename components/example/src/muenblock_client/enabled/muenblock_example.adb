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

with Interfaces;

with SK.Strings;

with Muenblock;

with Example_Component.Memory_Arrays;

with Log;
private with Muenblock_Example.Write_Ops;

package body Muenblock_Example
is

   -------------------------------------------------------------------------

   type Unsigned_48 is mod 2 ** 48;
   for Unsigned_48'Size use 48;

   SMART_Attribute_Size : constant := 12;

   type SMART_Attribute_Type is record
      ID       : Interfaces.Unsigned_8;
      Flags    : Interfaces.Unsigned_16;
      Current  : Interfaces.Unsigned_8;
      Worst    : Interfaces.Unsigned_8;
      Raw      : Unsigned_48;
      Reserved : Interfaces.Unsigned_8;
   end record
   with
      Size => SMART_Attribute_Size * 8;

   for SMART_Attribute_Type use record
      ID       at 0 range 0 .. 7;
      Flags    at 1 range 0 .. 15;
      Current  at 3 range 0 .. 7;
      Worst    at 4 range 0 .. 7;
      Raw      at 5 range 0 .. 6 * 8 - 1;
      Reserved at 11 range 0 .. 7;
   end record;

   type SMART_Attribute_Table_Type is
     array (Integer range 1 .. 30) of SMART_Attribute_Type
     with Object_Size => SMART_Attribute_Size * 30 * 8;

   -------------------------------------------
   -- NVME Figure 207: SMART / Health Info Log Page
   -------------------------------------------
   type Bit_Array is array (Natural range <>) of Boolean
   with Pack;

   type CriticalWarning_Type is record
      AvailableSpaceBelowThresh   : Boolean;
      TemperatureWarning          : Boolean; -- see Section 5.27.1.3
      ReliabilityDegraded         : Boolean;
      ReadOnlyModeActive          : Boolean; -- see Section 8.12.1
      BackupDeviceFailure         : Boolean;
      PersistMemoryRegionReadOnly : Boolean; -- see Section 8.14
   end record with
      Size => 8;
   for CriticalWarning_Type use record
      AvailableSpaceBelowThresh   at 0 range 0 .. 0;
      TemperatureWarning          at 0 range 1 .. 1;
      ReliabilityDegraded         at 0 range 2 .. 2;
      ReadOnlyModeActive          at 0 range 3 .. 3;
      BackupDeviceFailure         at 0 range 4 .. 4;
      PersistMemoryRegionReadOnly at 0 range 5 .. 5;
   end record;

   type TempSensorArray is array (1 .. 8) of Interfaces.Unsigned_16 with Pack; -- Consists of Temperature Readings [K]

   type SMART_LogPage is record
      CriticalWarning                  : CriticalWarning_Type; -- Indicated Type of Critical Warning
      CompositeTemperature             : Interfaces.Unsigned_16;          -- Current Composite Temp [Kelvin]
      AvailableSpare                   : Interfaces.Unsigned_8;           -- Current Available Space [%]
      AvailableSpareThreshold          : Interfaces.Unsigned_8;           -- Threshold for 'full' [%]
      PercentageUsed                   : Interfaces.Unsigned_8;           -- Estimate of NVM life used [%]
      -- Unused: EnduranceGroupCriticalWarning
      Reserved_1                       : Bit_Array (0 .. 207);
      DataUnitsRead                    : Interfaces.Unsigned_128;         -- Number of 512 Byte Units the Host has read
      DataUnitsWritten                 : Interfaces.Unsigned_128;         -- Number of 512 Byte Units the Host has written
      HostReadCommands                 : Interfaces.Unsigned_128;         -- Number of Host Read CMDs completed by the controller
      HostWriteommands                 : Interfaces.Unsigned_128;         -- Number of Host Write CMDs completed by the controller
      ControllerBusyTime               : Interfaces.Unsigned_128;         -- Amount of time the controller was busy with I/O CMDs [min]
      PowerCycles                      : Interfaces.Unsigned_128;         -- Number of Power Cycles
      PowerOnHours                     : Interfaces.Unsigned_128;         -- Number of (operational) Power-on hours [h]
      UnsafeShutdowns                  : Interfaces.Unsigned_128;         -- Number of unsafe shutdowns
      MediaAndIntegrityErrors          : Interfaces.Unsigned_128;         -- Number of detected unrecoverdd data integrity errors
      NumberOfErrorLogInfoEntries      : Interfaces.Unsigned_128;         -- Number of Error information log Entries
      WarningCompositeTempTime         : Interfaces.Unsigned_32;          -- Amount of time the Composite Temperarature was greater then allowed [min]
      CriticalCompositeTempTime        : Interfaces.Unsigned_32;          -- Amount of time the Composite Temperarature was critical [min]
      TempSensors                      : TempSensorArray;      -- Array for current temperarature reports by sensors 1 .. 8
      ThermalMngmtTemp1TransitionCount : Interfaces.Unsigned_32;          -- Number of times the controller thermal throttled lightly
      ThermalMngmtTemp2TransitionCount : Interfaces.Unsigned_32;          -- Number of times the controller thermal throttled heavily
      TotalThermalMngmtTempTime1       : Interfaces.Unsigned_32;          -- Amount of time the controller thermal throttled lightly [s]
      TotalThermalMngmtTempTime2       : Interfaces.Unsigned_32;          -- Amount of time the controller thermal throttled heavily [s]
      Reserved_2                       : Bit_Array (0 .. 2175);
   end record with
      Size => 512 * 8;
   for SMART_LogPage use record
      CriticalWarning                  at   0 range 0 ..    7;
      CompositeTemperature             at   1 range 0 ..   15;
      AvailableSpare                   at   3 range 0 ..    7;
      AvailableSpareThreshold          at   4 range 0 ..    7;
      PercentageUsed                   at   5 range 0 ..    7;
      Reserved_1                       at   6 range 0 ..  207;
      DataUnitsRead                    at  32 range 0 ..  127;
      DataUnitsWritten                 at  48 range 0 ..  127;
      HostReadCommands                 at  64 range 0 ..  127;
      HostWriteommands                 at  80 range 0 ..  127;
      ControllerBusyTime               at  96 range 0 ..  127;
      PowerCycles                      at 112 range 0 ..  127;
      PowerOnHours                     at 128 range 0 ..  127;
      UnsafeShutdowns                  at 144 range 0 ..  127;
      MediaAndIntegrityErrors          at 160 range 0 ..  127;
      NumberOfErrorLogInfoEntries      at 176 range 0 ..  127;
      WarningCompositeTempTime         at 192 range 0 ..   31;
      CriticalCompositeTempTime        at 196 range 0 ..   31;
      TempSensors                      at 200 range 0 ..  127;
      ThermalMngmtTemp1TransitionCount at 216 range 0 ..   31;
      ThermalMngmtTemp2TransitionCount at 220 range 0 ..   31;
      TotalThermalMngmtTempTime1       at 224 range 0 ..   31;
      TotalThermalMngmtTempTime2       at 228 range 0 ..   31;
      Reserved_2                       at 232 range 0 .. 2175;
   end record;

   -------------------------------------------------------------------------

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   SMART_Attribute_Table_Ahci : SMART_Attribute_Table_Type
   with
      Volatile,
      Async_Writers,
      Size    => SMART_Attribute_Size * 30 * 8,
      Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base + 2);

   SMART_Attribute_Table_Nvme : SMART_LogPage
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base + Example_Component.Memory_Arrays.Blockdev_Shm2_Element_Size + 2);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure SMART_Dump_Data (Array_Index : Positive)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
      use type Interfaces.Unsigned_8;
      Attribute : SMART_Attribute_Type;
   begin
      case Array_Index is
         when 1 =>
            for I in SMART_Attribute_Table_Type'Range loop
               Attribute := SMART_Attribute_Table_Ahci (I);

               if Attribute.ID /= 0 then
                  Log.Put_Line
                     (Item => "SMART Attribute ID: "
                        & SK.Strings.Img (Attribute.ID));
                  Log.Put_Line
                     (Item => " Flags: " & SK.Strings.Img (Attribute.Flags));
                  Log.Put_Line
                     (Item => " Current: " & SK.Strings.Img (Attribute.Current));
                  Log.Put_Line
                     (Item => " Worst: " & SK.Strings.Img (Attribute.Worst));
                  Log.Put_Line
                     (Item => " Raw: " & SK.Strings.Img
                        (Interfaces.Unsigned_64 (Attribute.Raw)));
               end if;
            end loop;
         when 2 =>
            declare
               LogPage : constant SMART_LogPage := SMART_Attribute_Table_Nvme;
            begin
               Log.Put_Line ("Available Spare %: " & SK.Strings.Img (Interfaces.Unsigned_32 (LogPage.AvailableSpare)));
               Log.Put_Line ("Lifetime Used %  : " & SK.Strings.Img (Interfaces.Unsigned_32 (LogPage.PercentageUsed)));
               Log.Put_Line ("Data Units R     : " & SK.Strings.Img (LogPage.DataUnitsRead));
               Log.Put_Line ("Data Units W     : " & SK.Strings.Img (LogPage.DataUnitsWritten));
               Log.Put_Line ("Power Cycles     : " & SK.Strings.Img (LogPage.PowerCycles));
               Log.Put_Line ("Power On Hours   : " & SK.Strings.Img (LogPage.PowerOnHours));
               Log.Put_Line ("Unsafe Shutdowns : " & SK.Strings.Img (LogPage.UnsafeShutdowns));
               Log.Put_Line ("Media Integ Errs : " & SK.Strings.Img (LogPage.MediaAndIntegrityErrors));
               Log.Put_Line ("Warn Temp Time   : " & SK.Strings.Img (LogPage.WarningCompositeTempTime));
               Log.Put_Line ("Crit Temp Time   : " & SK.Strings.Img (LogPage.CriticalCompositeTempTime));
            end;
         when others =>
            Log.Put_Line
            (Item => "SMART invalid array index:");
            return;
      end case;
   end SMART_Dump_Data;

   -------------------------------------------------------------------------

   procedure Show
   is
      use type Interfaces.Unsigned_64;

      Res : Interfaces.Unsigned_64;

      Sector_Cnt  : Interfaces.Unsigned_64;
      Sector_Size : Interfaces.Unsigned_64;
      Max_Sectors : Interfaces.Unsigned_64;
      Valid       : Boolean;
      Wrt_Success : Boolean;
   begin
      Log.Put_Line ("Muenblock example start");
      -- FIXME if one Client fails before the other, the second one won't get tested
      for Client in Client_Range loop
         Log.Put_Line ("Testing client " & SK.Strings.Img (Interfaces.Unsigned_64 (Client)));

         Muenblock_Clients_Instance.Init (Client => Client, Timeout_MS => 5000);

         Muenblock_Clients_Instance.Get_Device_Info
           (Client      => Client,
            Device_Id   => 0,
            Sector_Cnt  => Sector_Cnt,
            Sector_Size => Sector_Size,
            Max_Sectors => Max_Sectors,
            Valid       => Valid);

         if not Valid then
            Log.Put_Line ("Unable to get device Info");
            return;
         end if;

         Log.Put_Line
         ("Device found with " & SK.Strings.Img (Sector_Cnt)
            & " sectors, Sector_Size " & SK.Strings.Img (Sector_Size)
            & " and Max_Sectors " & SK.Strings.Img (Max_Sectors));

         --  get device health (SMART)
         Muenblock_Clients_Instance.Get_SMART
           (Client        => Client,
            Device_Id     => 0,
            Buffer_Offset => 0,
            Result        => Res);
         case Res is
            when 0 =>
               Log.Put_Line ("Unable to read SMART Data!");
            when Muenblock.SMART_OK =>
               Log.Put_Line ("SMART Status: OK!");
            when Muenblock.SMART_THRESHOLD_EXCEEDED =>
               Log.Put_Line ("SMART Status: Threshold Exceeded!");
            when Muenblock.SMART_UNDEFINED =>
               Log.Put_Line ("SMART Status: Undefined!");
            when others =>
               null;
         end case;

         if Res /= 0 then
            SMART_Dump_Data (Positive (Client));
         end if;

         if Sector_Size = 0 then
            Log.Put_Line ("Sector size is 0");
            return;
         end if;
      end loop;

      Muenblock_Example.Write_Ops.Run
        (Sector_Size => Sector_Size,
         Success     => Wrt_Success);

      if Wrt_Success then
         Log.Put_Line ("Muenblock example done");
      end if;
   end Show;

end Muenblock_Example;
