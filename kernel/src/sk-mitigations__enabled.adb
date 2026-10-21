--
--  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Crash_Audit_Types;
with SK.Dump;
with SK.Strings;

with Skp.Mitigations;

package body SK.Mitigations
is

   -------------------------------------------------------------------------

   procedure Check_MCU_Revision (Revision : Interfaces.Unsigned_32)
   is
      Audit    : SK.Crash_Audit.Entry_Type;
      Mit_Ctx  : SK.Crash_Audit_Types.Mitigations_Context_Type
        := SK.Crash_Audit_Types.Null_Mitigations_Context;
   begin
      if Revision < Skp.Mitigations.Min_MCU_Revision then
         Dump.Print_Message
           (Msg => "MCU: Microcode revision "
            & SK.Strings.Img (Item => Revision)
            & " below required minimum "
            & SK.Strings.Img
              (Item => Interfaces.Unsigned_64 (Skp.Mitigations.Min_MCU_Revision))
            & ", system init failure");
         Mit_Ctx :=
           (MCU_Revision     => Revision,
            Min_MCU_Revision => Interfaces.Unsigned_32
              (Skp.Mitigations.Min_MCU_Revision));
         SK.Crash_Audit.Allocate (Audit => Audit);
         SK.Crash_Audit.Set_Reason
           (Audit  => Audit,
            Reason => SK.Crash_Audit_Types.Mitigations_MCU_Floor);
         SK.Crash_Audit.Set_Mitigations_Context
           (Audit   => Audit,
            Context => Mit_Ctx);
         SK.Crash_Audit.Finalize (Audit => Audit);
      end if;
   end Check_MCU_Revision;

end SK.Mitigations;
