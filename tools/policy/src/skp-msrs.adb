--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
            return;
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
