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

package body Musinfo.Utils
is

   -------------------------------------------------------------------------

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
   is
      pragma Unreferenced (Name, Address, Size, Writable);
   begin
      Info.Channel_Info_Count := Info.Channel_Info_Count + 1;
      Info.Channels_Info (Info.Channel_Info_Count)
        := Create_Channel_Info
          (Has_Event  => Has_Event,
           Has_Vector => Has_Vector,
           Event      => Event,
           Vector     => Vector);
   end Append_Channel;

   -------------------------------------------------------------------------

   procedure Append_Memregion
     (Info       : in out Subject_Info_Type;
      Name       :        Name_Type;
      Address    :        Interfaces.Unsigned_64;
      Size       :        Interfaces.Unsigned_64;
      Writable   :        Boolean;
      Executable :        Boolean)
   is
   begin
      Info.Memregion_Count := Info.Memregion_Count + 1;
      Info.Memregions (Info.Memregion_Count)
        := Create_Memregion
          (Address    => Address,
           Size       => Size,
           Writable   => Writable,
           Executable => Executable);

      Info.Resource_Count := Info.Resource_Count + 1;
      Info.Resources (Info.Resource_Count)
        := Create_Resource
          (Name               => Name,
           Memregion_Index    => Info.Memregion_Count,
           Channel_Info_Index => No_Resource);
   end Append_Memregion;

   -------------------------------------------------------------------------

   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Info_Type
   is
   begin
      return Channel_Info : Channel_Info_Type := Null_Channel_Info do
         Channel_Info.Flags.Has_Event  := Has_Event;
         Channel_Info.Flags.Has_Vector := Has_Vector;
         Channel_Info.Event            := Event;
         Channel_Info.Vector           := Vector;
      end return;
   end Create_Channel_Info;

   -------------------------------------------------------------------------

   function Create_Memregion
     (Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Writable   : Boolean;
      Executable : Boolean)
      return Memregion_Type
   is
   begin
      return Memregion : Memregion_Type := Null_Memregion do
         Memregion.Address          := Address;
         Memregion.Size             := Size;
         Memregion.Flags.Writable   := Writable;
         Memregion.Flags.Executable := Executable;
      end return;
   end Create_Memregion;

   -------------------------------------------------------------------------

   function Create_Name (Str : String) return Name_Type
   is
      Name    : Name_Type := Null_Name;
      Cur_Idx : Positive  := Name_Index_Type'First;
   begin
      Name.Length := Str'Length;

      for Char of Str loop
         Name.Data (Cur_Idx) := Char;
         Cur_Idx             := Cur_Idx + 1;
      end loop;

      return Name;
   end Create_Name;

   -------------------------------------------------------------------------

   function Create_Resource
     (Name               : Name_Type;
      Memregion_Index    : Resource_Count_Type;
      Channel_Info_Index : Resource_Count_Type)
      return Resource_Type
   is
   begin
      return Resource : Resource_Type := Null_Resource do
         Resource.Name             := Name;
         Resource.Memregion_Idx    := Memregion_Index;
         Resource.Channel_Info_Idx := Channel_Info_Index;
      end return;
   end Create_Resource;

end Musinfo.Utils;
