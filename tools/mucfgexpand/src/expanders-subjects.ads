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

package Expanders.Subjects
is

   --  Add subject binary physical memory regions and mappings. Removes the
   --  binary element after processing.
   procedure Add_Binaries (Data : in out Muxml.XML_Data_Type);

   --  Add subject profile related XML elements. Removes the profile attribute
   --  after processing.
   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type);

   --  Add tau0 subject.
   procedure Add_Tau0 (Data : in out Muxml.XML_Data_Type);

   --  Add subject state mappings to subject monitors. Removes the monitor
   --  element after processing.
   procedure Handle_Monitors (Data : in out Muxml.XML_Data_Type);

   --  Add subject ids.
   procedure Add_Ids (Data : in out Muxml.XML_Data_Type);

   --  Add subject bootparams element where missing.
   procedure Add_Missing_Elements (Data : in out Muxml.XML_Data_Type);

end Expanders.Subjects;
