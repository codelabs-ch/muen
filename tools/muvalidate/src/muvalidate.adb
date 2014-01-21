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

with Ada.Command_Line;
with Ada.Exceptions;

with Mulog;
with Muxml;

with Validate.Command_Line;
with Validators.Memory;
with Validators.MSR;

procedure Muvalidate
is
begin
   Validate.Command_Line.Init (Description => "Muen policy validator");

   --  Register validators.

   Validate.Register
     (Validator => Validators.Memory.Physical_Memory_References'Access);
   Validate.Register
     (Validator => Validators.Memory.VMXON_Region_Presence'Access);
   Validate.Register
     (Validator => Validators.Memory.VMXON_Region_Size'Access);
   Validate.Register
     (Validator => Validators.Memory.VMCS_Region_Presence'Access);
   Validate.Register
     (Validator => Validators.Memory.VMCS_Region_Size'Access);
   Validate.Register
     (Validator => Validators.Memory.Physical_Address_Alignment'Access);
   Validate.Register
     (Validator => Validators.Memory.Virtual_Address_Alignment'Access);
   Validate.Register
     (Validator => Validators.Memory.Region_Size'Access);
   Validate.Register
     (Validator => Validators.MSR.Start_Smaller_End'Access);
   Validate.Register
     (Validator => Validators.MSR.Low_Or_High'Access);

   Validate.Run;

exception
   when E : Muxml.Processing_Error |
      Validators.Validation_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Validation failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Muvalidate;
