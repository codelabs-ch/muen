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

with SK;

package CPU_Values
is

   type CPUID_Values_Type is record
      EAX : SK.Word32;
      EBX : SK.Word32;
      ECX : SK.Word32;
      EDX : SK.Word32;
   end record;

   Null_CPUID_Values : constant CPUID_Values_Type := (others => 0);

   --  Return CPUID values of hardware target for given leaf/subleaf.
   --  Returns False if the leaf/subleaf is not found, or if the request
   --  is outside of the supported leaf range.
   procedure Get_CPUID_Values
     (Leaf     :     SK.Word32;
      Subleaf  :     SK.Byte;
      Result   : out CPUID_Values_Type;
      Success  : out Boolean);

private

   type CPUID_Entry_Type is record
      Leaf        : SK.Word32;
      Has_Subleaf : Boolean;
      Subleaf     : SK.Byte;
      EAX         : SK.Word32;
      EBX         : SK.Word32;
      ECX         : SK.Word32;
      EDX         : SK.Word32;
   end record;

end CPU_Values;
