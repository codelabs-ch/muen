--
--  Copyright (C) 2014  secunet Security Networks AG
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

package body Dbg.Byte_Arrays
is

   -------------------------------------------------------------------------

   procedure Clear_Byte_Array (Data : out Byte_Array)
   is
   begin
      for D of Data loop
         D := 0;
      end loop;
   end Clear_Byte_Array;

   -------------------------------------------------------------------------

   procedure Copy_Byte_Array
     (Src        :        Byte_Array;
      Src_First  :        Natural;
      Src_Length :        Natural;
      Dest       : in out Byte_Array;
      Index      :        Natural)
   is
   begin
      for I in Natural range 0 .. Src_Length - 1 loop
         Dest (Index + I) := Src (Src_First + I);
      end loop;
   end Copy_Byte_Array;

   -------------------------------------------------------------------------

   procedure Copy_Byte_Array_Inc
     (Src        :        Byte_Array;
      Src_First  :        Natural;
      Src_Length :        Natural;
      Dest       : in out Byte_Array;
      Index      : in out Natural)
   is
   begin
      Copy_Byte_Array
        (Src        => Src,
         Src_First  => Src_First,
         Src_Length => Src_Length,
         Dest       => Dest,
         Index      => Index);
      Index := Index + Src_Length;
   end Copy_Byte_Array_Inc;

   -------------------------------------------------------------------------

   function Equal
     (Data1 : Byte_Array;
      I1    : Natural;
      Len   : Natural;
      Data2 : Byte_Array;
      I2    : Natural)
      return Boolean
   is
      use type Interfaces.Unsigned_8;

      Result : Boolean;
   begin
      Result := True;

      for I in Natural range 0 .. Len - 1 loop
         if Data1 (I1 + I) /= Data2 (I2 + I) then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end Equal;

end Dbg.Byte_Arrays;
