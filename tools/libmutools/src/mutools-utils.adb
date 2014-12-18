--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with Mutools.Constants;

package body Mutools.Utils
is

   use type Interfaces.Unsigned_64;

   type Hex_Digits_Range is range 16#0# .. 16#f#;

   Digits_To_Char : constant array (Hex_Digits_Range) of Character
     := (16#0# => '0',
         16#1# => '1',
         16#2# => '2',
         16#3# => '3',
         16#4# => '4',
         16#5# => '5',
         16#6# => '6',
         16#7# => '7',
         16#8# => '8',
         16#9# => '9',
         16#a# => 'a',
         16#b# => 'b',
         16#c# => 'c',
         16#d# => 'd',
         16#e# => 'e',
         16#f# => 'f');

   -------------------------------------------------------------------------

   function Bit_Clear
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64
   is
   begin
      return Value and not (2 ** Natural (Pos));
   end Bit_Clear;

   -------------------------------------------------------------------------

   function Bit_Set
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64
   is
   begin
      return (Value or 2 ** Natural (Pos));
   end Bit_Set;

   -------------------------------------------------------------------------

   function Bit_Test
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Boolean
   is
   begin
      return (Value and 2 ** Natural (Pos)) /= 0;
   end Bit_Test;

   -------------------------------------------------------------------------

   function Capitalize (Str : String) return String
   is
      Result : String := Ada.Characters.Handling.To_Lower (Item => Str);
   begin
      Result (Result'First) := Ada.Characters.Handling.To_Upper
        (Item => Result (Result'First));
      return Result;
   end Capitalize;

   -------------------------------------------------------------------------

   function Decode_Entity_Name (Encoded_Str : String) return String
   is
      Udrl_Idx : constant Natural := Ada.Strings.Fixed.Index
        (Source  => Encoded_Str,
         Pattern => "|");
   begin
      return Encoded_Str (Encoded_Str'First .. Udrl_Idx - 1);
   end Decode_Entity_Name;

   -------------------------------------------------------------------------

   function Indent
     (N         : Positive := 1;
      Unit_Size : Positive := 3)
      return String
   is
      Result : constant String (1 .. N * Unit_Size) := (others => ' ');
   begin
      return Result;
   end Indent;

   -------------------------------------------------------------------------

   function Is_Managed_By_VMX
     (MSR                    : Interfaces.Unsigned_64;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
      return Boolean
   is
      use Mutools.Constants;

      Result : Boolean;
   begin

      --  Reference: Intel SDM Vol. 3C, section 26.3.2.1 and section 27.3.1.

      case MSR is
         when IA32_SYSENTER_CS
            | IA32_SYSENTER_ESP
            | IA32_SYSENTER_EIP
            | IA32_FS_BASE
            | IA32_GS_BASE          => Result := True;
         when IA32_DEBUGCTL         => Result := DEBUGCTL_Control;
         when IA32_PAT              => Result := PAT_Control;
         when IA32_PERF_GLOBAL_CTRL => Result := PERFGLOBALCTRL_Control;
         when IA32_EFER             => Result := EFER_Control;
         when others                => Result := False;
      end case;

      return Result;
   end Is_Managed_By_VMX;

   -------------------------------------------------------------------------

   function To_Hex
     (Number     : Interfaces.Unsigned_64;
      Normalize  : Boolean := True;
      Byte_Short : Boolean := False)
      return String
   is
      Num_Str  : String (1 .. 23);
      Pos      : Natural                := Num_Str'Last;
      Tmp      : Interfaces.Unsigned_64 := Number;
      Digit    : Natural                := 0;
      Diggroup : Positive               := 1;

      --  Convert given hex digit to character.
      function To_Hex_Digit (N : Interfaces.Unsigned_64) return Character
        with pre => N <= 16#F#;

      ----------------------------------------------------------------------

      function To_Hex_Digit (N : Interfaces.Unsigned_64) return Character
      is
      begin
         return Digits_To_Char (Hex_Digits_Range (N));
      end To_Hex_Digit;
   begin
      if Normalize then
         Diggroup := 4;
         Num_Str (Pos) := '#';
         Pos := Pos - 1;
      end if;

      if Byte_Short and Number <= 16#ff# then
         Diggroup := 2;
      end if;

      loop
         Num_Str (Pos) := To_Hex_Digit (Tmp mod 16);
         Tmp   := Tmp / 16;
         Pos   := Pos - 1;
         Digit := Digit + 1;
         exit when Tmp = 0 and Digit mod Diggroup = 0;

         if Normalize and Digit mod Diggroup = 0 then
            Num_Str (Pos) := '_';
            Pos := Pos - 1;
         end if;
      end loop;

      if Normalize then
         Num_Str (Pos - 2 .. Pos) := "16#";
         Pos := Pos - 3;
      end if;

      return Num_Str (Pos + 1 .. Num_Str'Last);
   end To_Hex;

end Mutools.Utils;
