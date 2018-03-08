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

with Ada.Text_IO;
with Ada.IO_Exceptions;

package body Decoder
is

   package Float_IO is new Ada.Text_IO.Float_IO (Num => Float);

   ----------------------------------------------------------------------------

   procedure Decode
   is
      use type Interfaces.Unsigned_8;

      State : State_Type := 0;
      Byte  : Byte_Type  := 0;
   begin
      loop
         declare
            Frequency : constant Float := Next_Frequency;
            Phase     : Phase_Type;
            Bit       : Bit_Type;
            Found     : Boolean;
         begin
            Symbol_From_Freq (Frequency => Frequency,
                              Found     => Found,
                              Phase     => Phase,
                              Bit       => Bit);

            if State = 8 then
               if Is_Byte_Sep (Frequency) then
                  Ada.Text_IO.Put (Character'Val (Integer (Byte)));

                  State := 0;
                  Byte  := 0;
               end if;
            elsif Found and then Phase = State mod 2 then
               Byte  := 2 * Byte + Bit;
               State := State + 1;
            elsif Is_Byte_Sep (Frequency) then
               State := 0;
               Byte  := 0;
            end if;
         end;
      end loop;

   exception
      when Decode_Exception => return;
   end Decode;

   ----------------------------------------------------------------------------

   function Is_Byte_Sep (Frequency : Float) return Boolean
   is (Is_Nearly (Frequency, Byte_Sep));

   ----------------------------------------------------------------------------

   function Is_Nearly (L, R : Float) return Boolean
   is (R - Epsilon <= L and then L <= R + Epsilon);

   ----------------------------------------------------------------------------

   function Next_Frequency return Float
   is
      Dummy, Freq : Float;
   begin
      Float_IO.Get (Dummy);

      Float_IO.Get (Freq);

      return Freq;
   exception
      when Ada.IO_Exceptions.End_Error | Ada.IO_Exceptions.Data_Error =>
         raise Decode_Exception;
   end Next_Frequency;

   ----------------------------------------------------------------------------

   procedure Symbol_From_Freq
     (Frequency :     Float;
      Found     : out Boolean;
      Phase     : out Phase_Type;
      Bit       : out Bit_Type)
   is
   begin
      Found := False;

      for P in Phase_Type loop
         for B in Bit_Type loop
            if Is_Nearly (Frequency, Code (P, B)) then
               Phase := P;
               Bit   := B;
               Found := True;
               return;
            end if;
         end loop;
      end loop;
   end Symbol_From_Freq;

end Decoder;
