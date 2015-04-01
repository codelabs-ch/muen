--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package VTd
is

   --  PCI Bus Device Function (BDF).
   type Bus_Range      is range 0 .. 255;
   type Device_Range   is range 0 .. 31;
   type Function_Range is range 0 .. 7;

   type BDF_Type is private;

   Null_BDF : constant BDF_Type;

private

   type Null_Bus_Range      is range -1 .. Bus_Range'Last;
   type Null_Device_Range   is range -1 .. Device_Range'Last;
   type Null_Function_Range is range -1 .. Function_Range'Last;

   type BDF_Type is record
      Bus    : Null_Bus_Range;
      Device : Null_Device_Range;
      Func   : Null_Function_Range;
   end record;

   Null_BDF : constant BDF_Type := (Bus => -1, Device => -1, Func => -1);

end VTd;
