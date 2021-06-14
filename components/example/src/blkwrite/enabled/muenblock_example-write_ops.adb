with SK.Strings;

with Example_Component.Memory;

with Log;

package body Muenblock_Example.Write_Ops
is

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
   Test_Write : Test_Data_Type
     with
       Volatile,
       Async_Readers,
       Address => System'To_Address
         (Example_Component.Memory.Blockdev_Shm2_Address);
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   Test_Read : Test_Data_Type
     with
       Volatile,
       Async_Writers,
       Address => System'To_Address
         (Example_Component.Memory.Blockdev_Shm2_Address
          + (Interfaces.Unsigned_64'Mod (Test_Data_Type'Size) / 8));
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Run
     (Sector_Size :     Interfaces.Unsigned_64;
      Success     : out Boolean)
   is
      Start_Time           : SK.Word64;
      End_Time             : SK.Word64;
      Res                  : Interfaces.Unsigned_64;
      Test_Data_Sector_Cnt : Interfaces.Unsigned_64;
   begin
      Success := False;

      Test_Data_Sector_Cnt := Interfaces.Unsigned_64'Mod
        (Test_Data_Type'Size) / 8 / Sector_Size;

      Muenblock_Client_Instance.Discard
        (Device_Id    => 0,
         Start_Sector => 0,
         Sector_Cnt   => Test_Data_Sector_Cnt,
         Result       => Res);
      if Res /= 0 then
         Log.Put_Line ("Discard failed!");
         return;
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
         Log.Put_Line ("Write failed! "
                       & SK.Strings.Img (Interfaces.Unsigned_32'Mod (Res)));
         return;
      else
         Log.Put_Line ("Wrote "
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
         Log.Put_Line ("Sync failed!");
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

      Log.Put_Line ("Read "
                    & SK.Strings.Img (Interfaces.Unsigned_32'Mod
                      (Test_Data_Type'Size / 8))
                    & " bytes in "
                    & SK.Strings.Img ((End_Time - Start_Time))
                    & " ticks");

      if Res /= Test_Data_Sector_Cnt then
         Log.Put_Line ("Read failed! "
                       & SK.Strings.Img (Interfaces.Unsigned_32'Mod (Res)));
         return;
      end if;

      declare
         R : Interfaces.Unsigned_64;
         W : Interfaces.Unsigned_64;
      begin
         for I in Test_Data_Type'Range loop
            R := Test_Read (I);
            W := Test_Write (I);
            if R /= W then
               Log.Put_Line
                 ("Data did not match! Offset:" &
                    SK.Strings.Img (Interfaces.Unsigned_32 (I)) & " Read : " &
                    SK.Strings.Img (R) & ". Wanted: " &
                    SK.Strings.Img (W)
                 );
               return;
            end if;
         end loop;
      end;

      Success := True;
   end Run;

end Muenblock_Example.Write_Ops;
