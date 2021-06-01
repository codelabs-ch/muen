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

with Example_Component.Memory;

with Log;
private with Muenblock_Example.Write_Ops;

package body Muenblock_Example
is

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
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   SMART_Attribute_Table : SMART_Attribute_Table_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address
         (Example_Component.Memory.Blockdev_Shm2_Address + 2);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure SMART_Dump_Data
   with
      Pre => Musinfo.Instance.Is_Valid
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
         SK.Strings.Img (Sector_Cnt) & " sectors and Sector_Size: " &
         SK.Strings.Img (Sector_Size));

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

      if Sector_Size = 0 then
         Log.Put_Line ("Sector size is 0");
         return;
      end if;

      Muenblock_Example.Write_Ops.Run
        (Sector_Size => Sector_Size,
         Success     => Wrt_Success);

      if Wrt_Success then
         Log.Put_Line ("Muenblock example done");
      end if;
   end Show;

end Muenblock_Example;
