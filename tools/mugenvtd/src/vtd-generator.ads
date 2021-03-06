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

package VTd.Generator
is

   --  Write VT-d tables as specified by policy to given output directory.
   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

private

   --  Write VT-d DMAR root table as specified by the policy to the given
   --  output directory.
   procedure Write_Root_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write VT-d DMAR context tables for each device security domain specified
   --  in the system policy to given output directory.
   procedure Write_Context_Tables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write device security domain pagetables as specified by the policy to
   --  the given output directory.
   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write VT-d IR table to the given output directory. Currently, a default
   --  table with two entries is written, both enries' Present flag is cleared.
   procedure Write_IR_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

end VTd.Generator;
