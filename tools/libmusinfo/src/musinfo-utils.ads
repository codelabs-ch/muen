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

   --  Create channel with given parameters.
   function Create_Channel
     (Name       : Name_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Writable   : Boolean;
      Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Type;

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
       Pre => Info.Channel_Count < Channel_Count_Type'Last;

end Musinfo.Utils;
