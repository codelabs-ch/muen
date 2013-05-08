with Ada.Unchecked_Conversion;

package body Skp.MSRs
is

   -------------------------------------------------------------------------

   procedure Allow_MSRs
     (Bitmap     : in out MSR_Bitmap_Type;
      Start_Addr :        SK.Word32;
      End_Addr   :        SK.Word32;
      Mode       :        MSR_Mode_Type)
   is
      use type SK.Word32;

      Mask   : constant := 16#1fff#;
      Offset : Natural;
   begin
      case Mode is
         when MSR_Read_Write =>
            Allow_MSRs (Bitmap     => Bitmap,
                        Start_Addr => Start_Addr,
                        End_Addr   => End_Addr,
                        Mode       => MSR_Read);
            Allow_MSRs (Bitmap     => Bitmap,
                        Start_Addr => Start_Addr,
                        End_Addr   => End_Addr,
                        Mode       => MSR_Write);
         when MSR_Write =>
            Offset := 2048 * 8;
         when MSR_Read =>
            Offset := 0;
      end case;

      if Start_Addr in MSR_High_Range then
         Offset := Offset + 1024 * 8;
      end if;

      for Addr in SK.Word32 range Start_Addr .. End_Addr loop
         Bitmap (Natural (Addr and Mask) + Offset) := Allowed;
      end loop;
   end Allow_MSRs;

   -------------------------------------------------------------------------

   function To_Stream (Bitmap : MSR_Bitmap_Type) return MSR_Bitmap_Stream
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => MSR_Bitmap_Type,
         Target => MSR_Bitmap_Stream);
   begin
      return Convert (S => Bitmap);
   end To_Stream;

end Skp.MSRs;
