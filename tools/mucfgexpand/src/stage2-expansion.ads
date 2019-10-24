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

with Muxml;
with Mutools.Processors;

package Stage2.Expansion
is

   --  Register stage 2 expanders.
   procedure Register_All;

   --  Run registered expanders.
   procedure Run (Data : in out Muxml.XML_Data_Type);

   --  Return number of registered expanders.
   function Get_Count return Natural;

   --  Clear registered expanders.
   procedure Clear;

private

   package Procs is new Mutools.Processors
     (Param_Type => Muxml.XML_Data_Type);

end Stage2.Expansion;
