--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

package body SK.UART_HS
is

   -------------------------------------------------------------------------

   procedure Read
     (Address :     Word64;
      Value   : out Word32)
   with
      SPARK_Mode => Off
   is
      HS_In : Word32
        with
          Volatile,
          Address => System'To_Address (Address);
   begin
      Value := HS_In;
   end Read;

   -------------------------------------------------------------------------

   procedure Write
     (Address : Word64;
      Value   : Word32)
   with
      SPARK_Mode => Off
   is
      HS_Out : Word32
        with
          Volatile,
          Address => System'To_Address (Address);
   begin
      HS_Out := Value;
   end Write;

end SK.UART_HS;
