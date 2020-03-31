--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;
use type Interfaces.Unsigned_32;

package body Ahci.HBA
is

   -------------------------------------------------------------------------

   procedure Enable
   is
      Ctrl     : Global_HBA_Control_Type := Instance.Global_Host_Control;
      Bohc     : BIOS_HO_Status_Type := Instance.BIOS_HO_Status_Ctrl;
      Caps_Ext : constant HBA_Caps_Ext_Type := Instance.Host_Capabilities_Ext;
   begin
      Ctrl.AE := True;
      Instance.Global_Host_Control := Ctrl;

      if Caps_Ext.BOH then
         --  request OS Ownership
         Bohc.OOS := True;
         Instance.BIOS_HO_Status_Ctrl := Bohc;
         Wait_Owner : loop
            Bohc := Instance.BIOS_HO_Status_Ctrl;
            exit Wait_Owner when Bohc.OOS and not Bohc.BOS;
         end loop Wait_Owner;
      end if;
   end Enable;

   -------------------------------------------------------------------------

   procedure Reset
   is
      Ctrl : Global_HBA_Control_Type := Instance.Global_Host_Control;
   begin
      Ctrl.AE := True;
      Instance.Global_Host_Control := Ctrl;

      Ctrl.HR := True;
      Instance.Global_Host_Control := Ctrl;

      Wait_Reset : loop
         Ctrl := Instance.Global_Host_Control;
         exit Wait_Reset when Ctrl.HR = False;
      end loop Wait_Reset;
   end Reset;

end Ahci.HBA;
