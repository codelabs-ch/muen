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
   -- Info: all spec information is about NVMe Base Specification 2.0c

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

   type CriticalWarning_Type is record
      Available_Space_Below_Thresh   : Boolean;
      Temperature_Warning            : Boolean; -- see Section 5.27.1.3
      Reliability_Degraded           : Boolean;
      ReadOnly_Mode_Active           : Boolean; -- see Section 8.12.1
      Backup_Device_Failure          : Boolean;
      Persist_Memory_Region_ReadOnly : Boolean; -- see Section 8.14
   end record
   with
      Size => 8;
   for CriticalWarning_Type use record
      Available_Space_Below_Thresh   at 0 range 0 .. 0;
      Temperature_Warning            at 0 range 1 .. 1;
      Reliability_Degraded           at 0 range 2 .. 2;
      ReadOnly_Mode_Active           at 0 range 3 .. 3;
      Backup_Device_Failure          at 0 range 4 .. 4;
      Persist_Memory_Region_ReadOnly at 0 range 5 .. 5;
   end record;

   -- Consists of Temperature Readings [K]
   type Temp_Sensor_Array is array (1 .. 8) of Interfaces.Unsigned_16
   with
      Pack;

   -- NVME Figure 207: SMART / Health Info Log Page
   type SMART_Log_Page is record
      Critical_Warning                     : CriticalWarning_Type;      -- Indicated Type of Critical Warning
      Composite_Temperature                : Interfaces.Unsigned_16;    -- Current Composite Temp [Kelvin]
      Available_Spare                      : Interfaces.Unsigned_8;     -- Current Available Space [%]
      Available_Spare_Threshold            : Interfaces.Unsigned_8;     -- Threshold for 'full' [%]
      Percentage_Used                      : Interfaces.Unsigned_8;     -- Estimate of NVM life used [%]
      -- Unused: EnduranceGroupCriticalWarning
      Reserved_1                           : SK.Bit_Array (1 .. 208);
      Data_Units_Read                      : Interfaces.Unsigned_128;   -- Number of 512 Byte Units the Host has read
      Data_Units_Written                   : Interfaces.Unsigned_128;   -- Number of 512 Byte Units the Host has written
      Host_Read_Commands                   : Interfaces.Unsigned_128;   -- Number of Host Read CMDs completed by the controller
      Host_Write_Commands                  : Interfaces.Unsigned_128;   -- Number of Host Write CMDs completed by the controller
      Controller_Busy_Time                 : Interfaces.Unsigned_128;   -- Amount of time the controller was busy with I/O CMDs [min]
      Power_Cycles                         : Interfaces.Unsigned_128;   -- Number of Power Cycles
      Power_On_Hours                       : Interfaces.Unsigned_128;   -- Number of (operational) Power-on hours [h]
      Unsafe_Shutdowns                     : Interfaces.Unsigned_128;   -- Number of unsafe shutdowns
      Media_And_Integrity_Errors           : Interfaces.Unsigned_128;   -- Number of detected unrecoverdd data integrity errors
      Number_Of_Error_Log_Info_Entries     : Interfaces.Unsigned_128;   -- Number of Error information log Entries
      Warning_Composite_Temp_Time          : Interfaces.Unsigned_32;    -- Amount of time the Composite Temperarature was greater then allowed [min]
      Critical_Composite_Temp_Time         : Interfaces.Unsigned_32;    -- Amount of time the Composite Temperarature was critical [min]
      Temp_Sensors                         : Temp_Sensor_Array;         -- Array for current temperarature reports by sensors 1 .. 8
      Thermal_Mngmt_Temp1_Transition_Count : Interfaces.Unsigned_32;    -- Number of times the controller thermal throttled lightly
      Thermal_Mngmt_Temp2_Transition_Count : Interfaces.Unsigned_32;    -- Number of times the controller thermal throttled heavily
      Total_Thermal_Mngmt_Temp_Time1       : Interfaces.Unsigned_32;    -- Amount of time the controller thermal throttled lightly [s]
      Total_Thermal_Mngmt_Temp_Time2       : Interfaces.Unsigned_32;    -- Amount of time the controller thermal throttled heavily [s]
      Reserved_2                           : SK.Bit_Array (1 .. 2176);
   end record
   with
      Size => 512 * 8;
   for SMART_Log_Page use record
      Critical_Warning                     at   0 range 0 ..    7;
      Composite_Temperature                at   1 range 0 ..   15;
      Available_Spare                      at   3 range 0 ..    7;
      Available_Spare_Threshold            at   4 range 0 ..    7;
      Percentage_Used                      at   5 range 0 ..    7;
      Reserved_1                           at   6 range 0 ..  207;
      Data_Units_Read                      at  32 range 0 ..  127;
      Data_Units_Written                   at  48 range 0 ..  127;
      Host_Read_Commands                   at  64 range 0 ..  127;
      Host_Write_Commands                  at  80 range 0 ..  127;
      Controller_Busy_Time                 at  96 range 0 ..  127;
      Power_Cycles                         at 112 range 0 ..  127;
      Power_On_Hours                       at 128 range 0 ..  127;
      Unsafe_Shutdowns                     at 144 range 0 ..  127;
      Media_And_Integrity_Errors           at 160 range 0 ..  127;
      Number_Of_Error_Log_Info_Entries     at 176 range 0 ..  127;
      Warning_Composite_Temp_Time          at 192 range 0 ..   31;
      Critical_Composite_Temp_Time         at 196 range 0 ..   31;
      Temp_Sensors                         at 200 range 0 ..  127;
      Thermal_Mngmt_Temp1_Transition_Count at 216 range 0 ..   31;
      Thermal_Mngmt_Temp2_Transition_Count at 220 range 0 ..   31;
      Total_Thermal_Mngmt_Temp_Time1       at 224 range 0 ..   31;
      Total_Thermal_Mngmt_Temp_Time2       at 228 range 0 ..   31;
      Reserved_2                           at 232 range 0 .. 2175;
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

   SMART_Attribute_Table_Nvme : SMART_Log_Page
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
               LogPage : constant SMART_Log_Page := SMART_Attribute_Table_Nvme;
            begin
               Log.Put_Line ("Available Spare %: " & SK.Strings.Img (Interfaces.Unsigned_32 (LogPage.Available_Spare)));
               Log.Put_Line ("Lifetime Used %  : " & SK.Strings.Img (Interfaces.Unsigned_32 (LogPage.Percentage_Used)));
               Log.Put_Line ("Data Units R     : " & SK.Strings.Img (LogPage.Data_Units_Read));
               Log.Put_Line ("Data Units W     : " & SK.Strings.Img (LogPage.Data_Units_Written));
               Log.Put_Line ("Power Cycles     : " & SK.Strings.Img (LogPage.Power_Cycles));
               Log.Put_Line ("Power On Hours   : " & SK.Strings.Img (LogPage.Power_On_Hours));
               Log.Put_Line ("Unsafe Shutdowns : " & SK.Strings.Img (LogPage.Unsafe_Shutdowns));
               Log.Put_Line ("Media Integ Errs : " & SK.Strings.Img (LogPage.Media_And_Integrity_Errors));
               Log.Put_Line ("Warn Temp Time   : " & SK.Strings.Img (LogPage.Warning_Composite_Temp_Time));
               Log.Put_Line ("Crit Temp Time   : " & SK.Strings.Img (LogPage.Critical_Composite_Temp_Time));
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
         else
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
