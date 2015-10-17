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

with Interfaces;

with Ada.Unchecked_Conversion;

package body Crypt.Hasher
is

   subtype SHA256_Message_Range is LSC.SHA256.Message_Index range
     0 .. LSC.SHA256.Message_Index (Crypt.Data_Range'Last) / 64 - 1;
   subtype SHA256_Message_Type is LSC.SHA256.Message_Type
     (SHA256_Message_Range);

   --  Convert crypter message data to SHA256 message.
   function To_SHA256_Msg is new Ada.Unchecked_Conversion
     (Source => Crypt.Data_Array,
      Target => SHA256_Message_Type);

   -------------------------------------------------------------------------

   procedure SHA256_Hash
     (Input  :     Crypt.Message_Type;
      Output : out Crypt.Message_Type)
   is
      LSC_Hash : LSC.SHA256.SHA256_Hash_Type;
   begin
      LSC_Hash := LSC.SHA256.Hash
        (Message => To_SHA256_Msg (S => Input.Data),
         Length  => LSC.Types.Word64 (Input.Size * 8));

      Output      := Crypt.Null_Message;
      Output.Size := 32;
      for I in LSC.SHA256.SHA256_Hash_Index loop
         Output.Data (Crypt.Data_Range (I * 4 + 1)) := SK.Byte'Mod
           (LSC_Hash (I));
         Output.Data (Crypt.Data_Range (I * 4 + 2)) := SK.Byte'Mod
           (Interfaces.Shift_Right (Value  => LSC_Hash (I),
                                    Amount => 8));
         Output.Data (Crypt.Data_Range (I * 4 + 3)) := SK.Byte'Mod
           (Interfaces.Shift_Right (Value  => LSC_Hash (I),
                                    Amount => 16));
         Output.Data (Crypt.Data_Range (I * 4 + 4)) := SK.Byte'Mod
           (Interfaces.Shift_Right (Value  => LSC_Hash (I),
                                    Amount => 24));
      end loop;
   end SHA256_Hash;

end Crypt.Hasher;
