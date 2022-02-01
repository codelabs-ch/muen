--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with CPU_Values.Target;

package body CPU_Values
is

   -------------------------------------------------------------------------

   procedure Get_CPUID_Values
     (Leaf    :     SK.Word32;
      Subleaf :     SK.Byte;
      Result  : out CPUID_Values_Type;
      Success : out Boolean)
   is
      use type SK.Byte;
      use type SK.Word32;
   begin
      Result  := Null_CPUID_Values;
      Success := False;

      for R of CPU_Values.Target.CPUID loop
         if R.Leaf = Leaf then
            if R.Has_Subleaf then
               if R.Subleaf = Subleaf then
                  Result  := (EAX => R.EAX,
                              EBX => R.EBX,
                              ECX => R.ECX,
                              EDX => R.EDX);
                  Success := True;
                  return;
               end if;
            else
               Result  := (EAX => R.EAX,
                           EBX => R.EBX,
                           ECX => R.ECX,
                           EDX => R.EDX);
               Success := True;
               return;
            end if;
         end if;
      end loop;
   end Get_CPUID_Values;

   -------------------------------------------------------------------------

   procedure Get_MSR_Value
     (Address :     SK.Word32;
      Regval  : out SK.Word64;
      Success : out Boolean)
   is
      use type SK.Word32;
   begin
      Success := False;
      Regval  := 0;

      for M of CPU_Values.Target.MSR loop
         if M.Addr = Address then
            Regval  := M.Regval;
            Success := True;
         end if;
      end loop;
   end Get_MSR_Value;

end CPU_Values;
