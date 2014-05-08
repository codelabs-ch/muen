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

with Interfaces.C;

with Musinfo;

package C_Imports
is

   function C_Assert_Name
     (Name : Musinfo.Name_Type)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_name";

   function C_Assert_Channel
     (Channel : Musinfo.Channel_Type)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_channel";

   function C_Assert_Subject_Info
     (Info : Musinfo.Subject_Info_Type)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_subject_info";

end C_Imports;
