--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Constants;
with SK.Strings;

with CPU_Values;

with Debug_Ops;

pragma $Release_Warnings
  (Off, "unit ""Sm_Component.Config"" is not referenced",
   Reason => "Only used to control debug output");
with Sm_Component.Config;
pragma $Release_Warnings
  (On, "unit ""Sm_Component.Config"" is not referenced");

package body Exit_Handlers.RDMSR
is

   use SK.Constants;
   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      use type SK.Word32;
      use type SK.Word64;

      Res    : Boolean;
      Regval : SK.Word64;

      RAX : constant SK.Word64 := State.Regs.RAX;
      RCX : constant SK.Word64 := State.Regs.RCX;
      RDX : constant SK.Word64 := State.Regs.RDX;
      MSR : constant SK.Word32 := SK.Word32'Mod (RCX);
   begin
      Action := Types.Subject_Continue;

      CPU_Values.Get_MSR_Value
        (Address => MSR,
         Regval  => Regval,
         Success => Res);

      if Res and then MSR = IA32_MISC_ENABLE then

         --  Bit  0: Fast string operations
         --  Bit 11: Branch Trace Storage Unavailable
         --  Bit 12: Precise Event Based Sampling (PEBS) Unavailable
         State.Regs.RAX := Regval and 16#1801#;
         State.Regs.RDX := 0;
      else
         pragma Debug (Sm_Component.Config.Debug_Rdmsr,
                       Debug_Ops.Put_Line
                         (Item => "RDMSR " & SK.Strings.Img (MSR)));
         State.Regs.RAX := RAX and not 16#ffff_ffff#;
         State.Regs.RDX := RDX and not 16#ffff_ffff#;
      end if;
   end Process;

end Exit_Handlers.RDMSR;
