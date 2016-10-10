--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Musinfo;

package Sinfo.Constants
is

   Null_Subject_Info : constant Musinfo.Subject_Info_Type
     := (Magic              => Musinfo.Muen_Subject_Info_Magic,
         Name               => Musinfo.Null_Name,
         Resource_Count     => Musinfo.No_Resource,
         Memregion_Count    => Musinfo.No_Resource,
         Channel_Info_Count => Musinfo.No_Resource,
         Dev_Info_Count     => Musinfo.No_Resource,
         Padding            => (others => 0),
         TSC_Khz            => 1000,
         TSC_Schedule_Start => 0,
         TSC_Schedule_End   => 0,
         Resources          => (others => Musinfo.Null_Resource),
         Memregions         => (others => Musinfo.Null_Memregion),
         Channels_Info      => (others => Musinfo.Null_Channel_Info),
         Dev_Info           => (others => Musinfo.Null_Dev_Info));

end Sinfo.Constants;
