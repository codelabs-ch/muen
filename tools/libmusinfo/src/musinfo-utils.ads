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

package Musinfo.Utils
is

   --  Create name from given string.
   function Create_Name (Str : String) return Name_Type
     with
       Pre => Str'Length in Name_Index_Type;

   --  Create memory region with given parameters.
   function Create_Memregion
     (Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Writable   : Boolean;
      Executable : Boolean)
      return Memregion_Type;

   --  Create channel information with given parameters.
   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Info_Type;

   --  Create resource with given parameters.
   function Create_Resource
     (Name               : Name_Type;
      Memregion_Index    : Resource_Count_Type;
      Channel_Info_Index : Resource_Count_Type)
      return Resource_Type;

   --  Append channel with specified parameters to given subject info.
   procedure Append_Channel
     (Info       : in out Subject_Info_Type;
      Name       :        Name_Type;
      Address    :        Interfaces.Unsigned_64;
      Size       :        Interfaces.Unsigned_64;
      Writable   :        Boolean;
      Has_Event  :        Boolean;
      Has_Vector :        Boolean;
      Event      :        Event_Number_Range;
      Vector     :        Vector_Range)
     with
       Pre => Info.Channel_Info_Count < Resource_Count_Type'Last;

end Musinfo.Utils;
