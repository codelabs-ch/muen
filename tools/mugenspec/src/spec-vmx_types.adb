--
--  Copyright (C) 2024  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml.Utils;

package body Spec.VMX_Types
is

   type RFLAGS_Flags_Type is
     (Carry,
      Parity,
      AuxiliaryCarry,
      Zero,
      Sign,
      Trap,
      InterruptEnable,
      Direction,
      Overflow,
      IOPrivilegeLevel,
      NestedTask,
      Resume,
      Virtual8086Mode,
      AlignmentCheck,
      VirtualInterruptFlag,
      VirtualInterruptPending,
      Identification);

   type RFLAGS_Flags_Map_Type is array (RFLAGS_Flags_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   IOPL_Bit_Pos : constant := 12;

   --  RFLAGS flag bit positions as specified by Intel SDM Vol. 3A, " 2.3 System
   --  Flags and Fields in the EFLAGS register".
   function Get_RFLAGS_Value is new Utils.To_Number
     (Bitfield_Type => RFLAGS_Flags_Type,
      Mapping_Type  => RFLAGS_Flags_Map_Type,
      Map           =>
        (Carry                   => 0,
         Parity                  => 2,
         AuxiliaryCarry          => 4,
         Zero                    => 6,
         Sign                    => 7,
         Trap                    => 8,
         InterruptEnable         => 9,
         Direction               => 10,
         Overflow                => 11,
         IOPrivilegeLevel        => IOPL_Bit_Pos,
         NestedTask              => 14,
         Resume                  => 16,
         Virtual8086Mode         => 17,
         AlignmentCheck          => 18,
         VirtualInterruptFlag    => 19,
         VirtualInterruptPending => 20,
         Identification          => 21));

   -------------------------------------------------------------------------

   function Get_RFLAGS
     (Fields  : DOM.Core.Node_List;
      Default : Interfaces.Unsigned_64 := 0)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      --  By default, RFLAGS only has the reserved bit 1 set and all others are
      --  0, see Intel SDM Vol. 3A, "9.1.1 Processor State After Reset".
      RFLAGS_RESERVED_VALUE : constant := 2#00000000000000000000000000000010#;

      IOPL_Mask : constant Interfaces.Unsigned_64
        := Interfaces.Shift_Left (Value  => 2#11#,
                                  Amount => IOPL_Bit_Pos);
      IOPL_Str : constant String
        := Muxml.Utils.Get_Element_Value (List => Fields,
                                          Tag  => "IOPrivilegeLevel");
      IOPL_Value : constant Interfaces.Unsigned_64
        := (if IOPL_Str'Length = 0 then 0
            else Interfaces.Shift_Left
              (Value  => Interfaces.Unsigned_64'Value (IOPL_Str),
               Amount => IOPL_Bit_Pos));
      Value : constant Interfaces.Unsigned_64
        := Get_RFLAGS_Value (Fields  => Fields,
                             Default => Default);
   begin

      --  Mask out IOPL two bit field and add the calculated value.
      --  Note that there is no need to clear the reserved RFLAG bits since they
      --  cannot be specified via XML elements.

      return (Value and not IOPL_Mask) or IOPL_Value or RFLAGS_RESERVED_VALUE;
   end Get_RFLAGS;

end Spec.VMX_Types;
