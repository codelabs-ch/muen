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

with Ahci;

package body Ahci.Commands
is

   -------------------------------------------------------------------------

   procedure Cmd_Slot_Prepare
      (Port_ID :        Port_Range;
       Len     : in out Interfaces.Unsigned_32;
       Address :        Interfaces.Unsigned_64;
       RW      :        RW_Type)
   is
      use type Interfaces.Unsigned_64;
      Address_Offset : Interfaces.Unsigned_32 := 0;
      Bytes          : Interfaces.Unsigned_32;
      Bytes_Per_Prdt : constant               := 16#400000#;
      FIS_Len        : constant               := 5; -- FIS Len -> 5DWord
      High           : Interfaces.Unsigned_32;
      Low            : Unsigned_31;
      Length         : Interfaces.Unsigned_32 := Len;
      Prdt_Len       : Interfaces.Unsigned_32;
   begin
      Command_Lists (Port_ID)(0).CFL := FIS_Len;
      Command_Lists (Port_ID)(0).W := (RW = Write);

      --  build prdt list
      --  each prdt can handle up to 4M
      --  Prdt_Len is the number of Prdts needed for the transfer
      Prdt_Len := (Length - 1) / Bytes_Per_Prdt + 1;

      if Prdt_Len > Prdt_Arr_Type'Length then
         Prdt_Len := 8;
         Length := Prdt_Len * Bytes_Per_Prdt;
      end if;
      Len := Length;

      --  clear byte count
      Command_Lists (Port_ID)(0).PRDBC := 0;
      --  setup number of entries in physical region descriptor table
      Command_Lists (Port_ID)(0).PRDTL :=
         Interfaces.Unsigned_16 (Prdt_Len);

      --  fill physical region descriptor table
      for I in Integer range 0 .. Integer (Prdt_Len - 1) loop
         if Length < Bytes_Per_Prdt then
            Bytes := Length;
         else
            Bytes := Bytes_Per_Prdt;
         end if;
         Low  := Unsigned_31
            (Interfaces.Shift_Right
               (Address + Interfaces.Unsigned_64 (Address_Offset), 1));
         High := Interfaces.Unsigned_32 (Interfaces.Shift_Right
                  (Address + Interfaces.Unsigned_64 (Address_Offset), 32));

         Command_Table (Port_ID).Prdt (I).DBA := Low;
         Command_Table (Port_ID).Prdt (I).DBAU := High;
         Command_Table (Port_ID).Prdt (I).DBC
            := Unsigned_22 ((Bytes - 1) and 16#3f_ffff#);

         Length := Length - Bytes;
         Address_Offset := Address_Offset + Bytes;
      end loop;

   end Cmd_Slot_Prepare;

end Ahci.Commands;
