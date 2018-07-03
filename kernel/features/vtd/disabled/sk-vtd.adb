--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.VTd
with
  SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.VTd_Init_Context_Type)
   is
   begin
      Is_Valid := True;
      Ctx      := Crash_Audit_Types.Null_VTd_Init_Context;
   end Check_State;

   -------------------------------------------------------------------------

   procedure Process_Fault
   with
      SPARK_Mode => Off
   is
   begin
      null;
   end Process_Fault;

   -------------------------------------------------------------------------

   procedure Initialize
   with
      SPARK_Mode => Off
   is
   begin
      null;
   end Initialize;

end SK.VTd;
