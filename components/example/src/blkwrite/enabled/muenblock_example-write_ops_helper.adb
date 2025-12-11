with SK.Strings;

with Example_Component.Memory;

with Log;
package body Muenblock_Example.Write_Ops_Helper
is
   NVME_NAME : constant := "blockdev_NVME_shm2";
   AHCI_NAME : constant := "blockdev_AHCI_shm2";

   Test_Data_Size : constant := 32768;

   type Test_Data_Type
   is array (Integer range 0 .. Test_Data_Size - 1)
     of Interfaces.Unsigned_64
       with
         Object_Size => Test_Data_Size * 64;

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Test_Write_Ahci : Test_Data_Type
     with
       Volatile,
       Async_Readers,
       Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base);

   Test_Write_Nvme : Test_Data_Type
     with
       Volatile,
       Async_Readers,
       Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base + Example_Component.Memory_Arrays.Blockdev_Shm2_Element_Size);
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   Test_Read_Ahci : Test_Data_Type
     with
       Volatile,
       Async_Writers,
       Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base
          + (Interfaces.Unsigned_64'Mod (Test_Data_Type'Size) / 8));

   Test_Read_Nvme : Test_Data_Type
     with
       Volatile,
       Async_Writers,
       Address => System'To_Address
         (Example_Component.Memory_Arrays.Blockdev_Shm2_Address_Base + Example_Component.Memory_Arrays.Blockdev_Shm2_Element_Size
          + (Interfaces.Unsigned_64'Mod (Test_Data_Type'Size) / 8));
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Run_Instance
      (SHM_Array_Index :     Positive;
       Sector_Size     :     Interfaces.Unsigned_64;
       Success         : out Boolean)
   is
      Test_Data_Sector_Cnt : Interfaces.Unsigned_64;
      Start_Time           : SK.Word64;
      End_Time             : SK.Word64;
      Res                  : Interfaces.Unsigned_64;
      Instance_Name        : constant := Example_Component.Memory_Arrays.Blockdev_Shm2_Names (SHM_Array_Index);
   begin
      Success := False;

      -- Check for correct order of shm regions
      if Example_Component.Memory_Arrays.Blockdev_Shm2_Names (1) /= AHCI_NAME and
         Example_Component.Memory_Arrays.Blockdev_Shm2_Names (2) /= NVME_NAME then
         Log.Put_Line ("Invalid Order of BLOCKDEV_SHM2 Names; Aborting ...");
         return;
      end if;

      if SHM_Array_Index not in 1 | 2 then
         Log.Put_Line ("Invalid SHM Index: " & SK.Strings.Img (SHM_Array_Index));
         return;
      end if;

      Log.Put_Line ("Starting test of " & Instance_Name);

      Test_Data_Sector_Cnt := Interfaces.Unsigned_64'Mod
        (Test_Data_Type'Size) / 8 / Sector_Size;

      Muenblock_Client_Instance.Discard
        (Device_Id    => 0,
         Start_Sector => 0,
         Sector_Cnt   => Test_Data_Sector_Cnt,
         Result       => Res);
      if Res /= 0 then
         Log.Put_Line (Instance_Name & ": Discard failed!");
         return;
      end if;

      if SHM_Array_Index = 1 then
         for I in Test_Data_Type'Range loop
            Test_Write_Ahci (I) := Interfaces.Unsigned_64 (I);
         end loop;
      else
         for I in Test_Data_Type'Range loop
            Test_Write_Nvme (I) := Interfaces.Unsigned_64 (I);
         end loop;
      end if;

      Start_Time := Musinfo.Instance.TSC_Schedule_End;
      Muenblock_Client_Instance.Write
        (Device_Id     => 0,
         Start_Sector  => 0,
         Buffer_Offset => 0,
         Sector_Cnt    => Test_Data_Sector_Cnt,
         Result        => Res);

      End_Time := Musinfo.Instance.TSC_Schedule_Start;

      if Res /= Test_Data_Sector_Cnt then
         Log.Put_Line (Instance_Name & ": Write failed! "
                       & SK.Strings.Img (Interfaces.Unsigned_32'Mod (Res)));
         return;
      else
         Log.Put_Line (Instance_Name & ": Wrote "
                       & SK.Strings.Img (Interfaces.Unsigned_32'Mod
                         (Test_Data_Type'Size / 8))
                       & " bytes in "
                       & SK.Strings.Img ((End_Time - Start_Time))
                       & " ticks");
      end if;

      Muenblock_Client_Instance.Sync
        (Device_Id => 0,
         Result    => Res);

      if Res /= 0 then
         Log.Put_Line (Instance_Name & ": Sync failed!");
         return;
      end if;

      Start_Time := Musinfo.Instance.TSC_Schedule_End;
      Muenblock_Client_Instance.Read
        (Device_Id     => 0,
         Start_Sector  => 0,
         Buffer_Offset => Interfaces.Unsigned_64'Mod (Test_Data_Type'Size) / 8,
         Sector_Cnt    => Test_Data_Sector_Cnt,
         Result        => Res);

      End_Time := Musinfo.Instance.TSC_Schedule_Start;

      Log.Put_Line (Instance_Name & ": Read "
                    & SK.Strings.Img (Interfaces.Unsigned_32'Mod
                      (Test_Data_Type'Size / 8))
                    & " bytes in "
                    & SK.Strings.Img ((End_Time - Start_Time))
                    & " ticks");

      if Res /= Test_Data_Sector_Cnt then
         Log.Put_Line (Instance_Name & ": Read failed! "
                       & SK.Strings.Img (Interfaces.Unsigned_32'Mod (Res)));
         return;
      end if;

      declare
         R : Interfaces.Unsigned_64;
         W : Interfaces.Unsigned_64;
      begin
         if SHM_Array_Index = 1 then
            for I in Test_Data_Type'Range loop
               R := Test_Read_Ahci (I);
               W := Test_Write_Ahci (I);
               if R /= W then
                  Log.Put_Line
                  (Instance_Name & ": Data did not match! Offset:" &
                     SK.Strings.Img (Interfaces.Unsigned_32 (I)) & " Read : " &
                     SK.Strings.Img (R) & ". Wanted: " &
                     SK.Strings.Img (W)
                  );
                  return;
               end if;
            end loop;
         else
            for I in Test_Data_Type'Range loop
               R := Test_Read_Nvme (I);
               W := Test_Write_Nvme (I);
               if R /= W then
                  Log.Put_Line
                  (Instance_Name & ": Data did not match! Offset:" &
                     SK.Strings.Img (Interfaces.Unsigned_32 (I)) & " Read : " &
                     SK.Strings.Img (R) & ". Wanted: " &
                     SK.Strings.Img (W)
                  );
                  return;
               end if;
            end loop;
         end if;
      end;

      Log.Put_Line ("Finished test of " & Instance_Name & " successfully.");
      Success := True;
   end;
end Muenblock_Example.Write_Ops_Helper;