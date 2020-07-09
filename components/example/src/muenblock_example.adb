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

with Example_Component.Memory;

with SK.Strings;
with Interfaces;
with Log;

with Musinfo.Instance;
with Muenblock;

package body Muenblock_Example
is
   type Test_Data_Type
   is array (Integer range 0 .. 32767)
      of Interfaces.Unsigned_64;
   Test_Write : Test_Data_Type
   with
      Volatile,
      Async_Readers,
      Address => System'To_Address
            (Example_Component.Memory.Blockdev_Shm2_Address);

   Test_Read : Test_Data_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address
            (Example_Component.Memory.Blockdev_Shm2_Address
               + (Test_Data_Type'Size / 8));

   -------------------------------------------------------------------------

   type Unsigned_48 is mod 2 ** 48;
   for Unsigned_48'Size use 48;

   type SMART_Attribute_Type is record
      ID       : Interfaces.Unsigned_8;
      Flags    : Interfaces.Unsigned_16;
      Current  : Interfaces.Unsigned_8;
      Worst    : Interfaces.Unsigned_8;
      Raw      : Unsigned_48;
      Reserved : Interfaces.Unsigned_8;
   end record
   with
      Size => 12 * 8;

   for SMART_Attribute_Type use record
      ID       at 0 range 0 .. 7;
      Flags    at 1 range 0 .. 15;
      Current  at 3 range 0 .. 7;
      Worst    at 4 range 0 .. 7;
      Raw      at 5 range 0 .. 6 * 8 - 1;
      Reserved at 11 range 0 .. 7;
   end record;

   type SMART_Attribute_Table_Type is
      array (Integer range 1 .. 30) of SMART_Attribute_Type;
   SMART_Attribute_Table : SMART_Attribute_Table_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address
         (Example_Component.Memory.Blockdev_Shm2_Address + 2);

   procedure SMART_Dump_Data
   is
      use type Interfaces.Unsigned_8;
      Attribute : SMART_Attribute_Type;
   begin
      for I in SMART_Attribute_Table_Type'Range loop
         Attribute := SMART_Attribute_Table (I);
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
   end SMART_Dump_Data;

   procedure Show
   is
      use type Interfaces.Unsigned_64;
      Res : Interfaces.Unsigned_64 := 0;
      Start_Time : SK.Word64;
      End_Time   : SK.Word64;

      Sector_Cnt  : Interfaces.Unsigned_64;
      Sector_Size : Interfaces.Unsigned_64;
      Max_Sectors : Interfaces.Unsigned_64;
      Valid       : Boolean;

      Test_Data_Sector_Cnt : Interfaces.Unsigned_64;
   begin
      Log.Put_Line ("Muenblock example start");
      Muenblock_Client_Instance.Init (Timeout_MS => 5000);

      Muenblock_Client_Instance.Get_Device_Info
         (Device_Id   => 0,
          Sector_Cnt  => Sector_Cnt,
          Sector_Size => Sector_Size,
          Max_Sectors => Max_Sectors,
          Valid       => Valid);

      if not Valid then
         Log.Put_Line ("Unable to get device Info");
         return;
      end if;

      Log.Put_Line ("Device found with " &
         SK.Strings.Img (Sector_Cnt) & "Sectors. Sector_Size: " &
         SK.Strings.Img (Sector_Size));
      Test_Data_Sector_Cnt := Test_Data_Type'Size / 8 / Sector_Size;

      --  get device health (SMART)
      Muenblock_Client_Instance.Get_SMART
         (Device_Id     => 0,
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
         SMART_Dump_Data;
      end if;

      Muenblock_Client_Instance.Discard
         (Device_Id     => 0,
          Start_Sector  => 0,
          Sector_Cnt    => Test_Data_Sector_Cnt,
          Result        => Res);
      if Res /= 0 then
         Log.Put_Line ("Discard failed!");
      end if;

      for I in Test_Data_Type'Range loop
         Test_Write (I) := Interfaces.Unsigned_64 (I);
      end loop;

      Start_Time := Musinfo.Instance.TSC_Schedule_End;
      Muenblock_Client_Instance.Write
         (Device_Id     => 0,
          Start_Sector  => 0,
          Buffer_Offset => 0,
          Sector_Cnt    => Test_Data_Sector_Cnt,
          Result        => Res);

      End_Time := Musinfo.Instance.TSC_Schedule_Start;

      if Res /= Test_Data_Sector_Cnt then
         Log.Put_Line ("Write failed! " &
            SK.Strings.Img (Interfaces.Unsigned_32 (Res)));
      else
         Log.Put_Line ("Wrote " &
            SK.Strings.Img (Interfaces.Unsigned_32 (Test_Data_Type'Size / 8)) &
            "Bytes in " &
            SK.Strings.Img ((End_Time - Start_Time))
            & "Ticks.");
      end if;

      Muenblock_Client_Instance.Sync
         (Device_Id  => 0,
          Result     => Res);

      if Res /= 0 then
         Log.Put_Line ("Sync failed!");
      end if;

      Start_Time := Musinfo.Instance.TSC_Schedule_End;
      Muenblock_Client_Instance.Read
         (Device_Id     => 0,
          Start_Sector  => 0,
          Buffer_Offset => Test_Data_Type'Size / 8,
          Sector_Cnt    => Test_Data_Sector_Cnt,
          Result        => Res);

      End_Time := Musinfo.Instance.TSC_Schedule_Start;

      Log.Put_Line ("Read " &
         SK.Strings.Img (Interfaces.Unsigned_32 (Test_Data_Type'Size / 8)) &
         "Bytes in " &
         SK.Strings.Img ((End_Time - Start_Time))
         & "Ticks.");

      if Res /= Test_Data_Sector_Cnt then
         Log.Put_Line ("Read failed! " &
            SK.Strings.Img (Interfaces.Unsigned_32 (Res)));
      end if;

      declare
         R : Interfaces.Unsigned_64;
         W :  Interfaces.Unsigned_64;
      begin
         for I in Test_Data_Type'Range loop
            R := Test_Read (I);
            W := Test_Write (I);
            if R /= W then
               Log.Put_Line ("Data did not match! Offset:" &
                     SK.Strings.Img (Interfaces.Unsigned_32 (I)) & " Read : " &
                     SK.Strings.Img (R) & ". Wanted: " &
                     SK.Strings.Img (W)
                  );
               return;
            end if;
         end loop;
      end;
      Log.Put_Line ("Muenblock example done");
   end Show;

end Muenblock_Example;
