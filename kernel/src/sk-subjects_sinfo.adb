--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Skp.Kernel;

with Musinfo;

package body SK.Subjects_Sinfo
with
   Refined_State => (State => Sinfo)
is

   pragma Warnings
     (Off,
      "component size overrides size clause for ""Subject_Info_Type""",
      Reason => "Reserved memory size is bigger than actual size of type");
   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Sinfo_Array is array
     (Skp.Subject_Id_Type) of Musinfo.Subject_Info_Type
     with
       Independent_Components,
       Component_Size => Skp.Kernel.Subj_Sinfo_Size * 8,
       Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");
   pragma Warnings
     (On,
      "component size overrides size clause for ""Subject_Info_Type""");

   --  Subject sinfo regions.
   Sinfo : Sinfo_Array
     with
       Address => System'To_Address (Skp.Kernel.Subj_Sinfo_Address);

   -------------------------------------------------------------------------

   procedure Export_Scheduling_Info
     (Id                 : Skp.Subject_Id_Type;
      TSC_Schedule_Start : SK.Word64;
      TSC_Schedule_End   : SK.Word64)
   is
   begin
      Sinfo (Id).TSC_Schedule_Start := Interfaces.Unsigned_64
        (TSC_Schedule_Start);
      Sinfo (Id).TSC_Schedule_End   := Interfaces.Unsigned_64
        (TSC_Schedule_End);
   end Export_Scheduling_Info;

begin

   --  FIXME: Initialization of "Sinfo" hidden.

   pragma SPARK_Mode (Off);

end SK.Subjects_Sinfo;
