--
--  Copyright (C) 2018  secunet Security Networks AG
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

with HW.Time;

with SK.IO;

package body Dbg.PC_Speaker
is

   PIT_Data    : constant := 16#42#; --  PIT Channel 2 data port
   PIT_Gate    : constant := 16#61#; --  Gate port for PIT_Data
   PIT_Command : constant := 16#43#; --  Mode/Command register

   Code : constant array (Phase, Bit) of Interfaces.Unsigned_32
     := (0 => (200, 700),
         1 => (1100, 1700));

   Byte_Sep : constant := 2600;

   ----------------------------------------------------------------------------

   function Nth_Bit
     (B : Interfaces.Unsigned_8;
      N : Pos)
      return Bit
   is
      use type Interfaces.Unsigned_8;
   begin
      return (if (B and Interfaces.Unsigned_8 (2 ** Natural (N))) /= 0
              then 1 else 0);
   end Nth_Bit;

   ----------------------------------------------------------------------------

   procedure Play_Sound
     (Frequency   : Interfaces.Unsigned_32;
      Duration_MS : Natural)
   is
   begin
      Start_Beep (Frequency => Frequency);
      HW.Time.M_Delay (MS => Duration_MS);
      Stop_Beep;

      --  Allow the speaker to turn off again.

      HW.Time.M_Delay (MS => Duration_MS);
   end Play_Sound;

   ----------------------------------------------------------------------------

   procedure Put
     (Item        : Byte_Arrays.Byte_Array;
      Duration_MS : Natural)
   is
   begin
      for B of Item loop
         Put_Byte (Item        => B,
                   Duration_MS => Duration_MS);
      end loop;
   end Put;

   ----------------------------------------------------------------------------

   procedure Put_Byte
     (Item        : Interfaces.Unsigned_8;
      Duration_MS : Natural)
   is
      use type Pos;
   begin
      for N in Pos'First .. Pos'Last loop
         Play_Sound
           (Frequency   => Code (N mod 2, Nth_Bit (Item, 7 - N)),
            Duration_MS => Duration_MS);
      end loop;

      Play_Sound (Frequency => Byte_Sep, Duration_MS => 2 * Duration_MS);
   end Put_Byte;

   ----------------------------------------------------------------------------

   procedure Start_Beep (Frequency : Interfaces.Unsigned_32)
   is
      use Interfaces;

      Div_Frequency : constant Unsigned_32 := 1193180 / Frequency;
   begin
      SK.IO.Outb (Port  => PIT_Command,
                  Value => 16#b6#);
      SK.IO.Outb (Port  => PIT_Data,
                  Value => Unsigned_8 (Div_Frequency and 16#ff#));
      SK.IO.Outb (Port  => PIT_Data,
                  Value => Unsigned_8
                    (Shift_Right (Value  => Div_Frequency,
                                  Amount => 8) and 16#ff#));

      declare
         Old_Gate : Interfaces.Unsigned_8;
      begin
         SK.IO.Inb (Port  => PIT_Gate,
                    Value => Old_Gate);

         if Old_Gate /= (Old_Gate or 3) then
            SK.IO.Outb (Port  => PIT_Gate,
                        Value => Old_Gate or 3);
         end if;
      end;
   end Start_Beep;

   ----------------------------------------------------------------------------

   procedure Stop_Beep
   is
      use type Interfaces.Unsigned_8;

      Old_Gate : Interfaces.Unsigned_8;
   begin
      SK.IO.Inb (Port  => PIT_Gate,
                 Value => Old_Gate);
      SK.IO.Outb (Port  => PIT_Gate,
                  Value => Old_Gate and 16#FC#);
   end Stop_Beep;

end Dbg.PC_Speaker;
