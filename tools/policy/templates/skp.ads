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

package Skp is

   type CPU_Range is range __cpu_range__;
   for CPU_Range'Size use 8;

   subtype Subject_Id_Type is Natural range __subj_range__;

   Invalid_Subject : constant := Subject_Id_Type'Last + 1;

   subtype Dst_Subject_Type is Natural range 0 .. Invalid_Subject;

   type Vector_Range is range 0 .. 255;

   Invalid_Vector : constant := 256;

   type Dst_Vector_Range is range 0 .. Invalid_Vector;

   Vmxon_Address : constant := 16#__vmxon_addr__#;

end Skp;
