--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mucontrol.Command;

package Ctrlr.Commands
is

   --  Initialize command interface of subject specified by ID with given
   --  command, epoch and watchdog interval.
   procedure Initialize
     (ID          : Managed_Subjects_Range;
      Command     : Mucontrol.Command.Command_Type;
      Epoch       : Interfaces.Unsigned_64;
      WD_Interval : Interfaces.Unsigned_64 := Mucontrol.Command.WD_DISABLED);

   --  Returns the current command for subject specified by ID.
   function Get_Command
     (ID : Managed_Subjects_Range)
      return Mucontrol.Command.Command_Type
     with Volatile_Function;

   --  Sets the command of subject specified by ID to the given value.
   procedure Set_Command
     (ID    : Managed_Subjects_Range;
      Value : Mucontrol.Command.Command_Type);

   --  Returns the current epoch for subject specified by ID.
   function Get_Epoch
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
     with Volatile_Function;

   --  Sets the epoch of subject specified by ID to the given value.
   procedure Set_Epoch
     (ID    : Managed_Subjects_Range;
      Value : Interfaces.Unsigned_64);

   --  Returns the current watchdog interval for subject specified by ID.
   function Get_Watchdog_Interval
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
     with Volatile_Function;

   --  Sets the watchdog interval of subject specified by ID to the given
   --  value.
   procedure Set_Watchdog_Interval
     (ID    : Managed_Subjects_Range;
      Value : Interfaces.Unsigned_64);

private

   use type Interfaces.Unsigned_64;

   package Cspecs renames Controller_Component.Memory_Arrays;

   Array_Size : constant Interfaces.Unsigned_64
     := Interfaces.Unsigned_64 (Managed_Subjects_Range'Last) * Page_Size * 8;

   type Command_Array is array (Managed_Subjects_Range)
     of Mucontrol.Command.Command_Interface_Type
       with
         Size        => Array_Size,
         Object_Size => Array_Size;

   Command_Pages : Command_Array
     with
       Import,
       Async_Readers,
       Size    => Array_Size,
       Address => System'To_Address (Cspecs.Control_Address_Base);

end Ctrlr.Commands;
