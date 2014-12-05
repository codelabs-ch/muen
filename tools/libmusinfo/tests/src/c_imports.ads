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

with System;

with Interfaces.C;

package C_Imports
is

   function C_Assert_Name
     (Name : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_name";

   function C_Assert_Channel_Info
     (Channel_Info : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_channel_info";

   function C_Assert_Resource
     (Resource : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_resource";

   function C_Assert_Subject_Info
     (Info : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_subject_info";

   function C_Assert_Name_Type
     (Size          : Interfaces.C.int;
      Alignment     : Interfaces.C.int;
      Length_Offset : Interfaces.C.int;
      Data_Offset   : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_name_type";

   function C_Assert_Channel_Type
     (Size           : Interfaces.C.int;
      Alignment      : Interfaces.C.int;
      Name_Offset    : Interfaces.C.int;
      Address_Offset : Interfaces.C.int;
      Size_Offset    : Interfaces.C.int;
      Flags_Offset   : Interfaces.C.int;
      Event_Offset   : Interfaces.C.int;
      Vector_Offset  : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_channel_type";

   function C_Assert_Resource_Type
     (Size                    : Interfaces.C.int;
      Alignment               : Interfaces.C.int;
      Name_Offset             : Interfaces.C.int;
      Memregion_Idx_Offset    : Interfaces.C.int;
      Channel_Info_Idx_Offset : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_resource_type";


   function C_Assert_Subject_Info_Type
     (Size                 : Interfaces.C.int;
      Alignment            : Interfaces.C.int;
      Magic_Offset         : Interfaces.C.int;
      Channel_Count_Offset : Interfaces.C.int;
      TSC_Khz_Offset       : Interfaces.C.int;
      Channels_Offset      : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_subject_info_type";

end C_Imports;
