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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Mutools.Constants;
with Mutools.Types;

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
      Result : String := Str;
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

   function Get_Event_Kind_Types_String return String
   is
      use type Types.Event_Action_Kind;
      use Ada.Strings.Unbounded;

      Result : Unbounded_String
        := Indent & To_Unbounded_String ("type Event_Action_Kind is");
   begin
      for T in Types.Event_Action_Kind'Range loop
         Result := Result & ASCII.LF & Indent & "  "
           & (if T = Types.Event_Action_Kind'First then "(" else " ")
           & To_Ada_Identifier (Str => T'Img)
           & (if T = Types.Event_Action_Kind'Last then ");" else ",");
      end loop;
      Result := Result & ASCII.LF  & ASCII.LF;

      Result := Result & Indent
        & "subtype Source_Event_Action_Kind is Event_Action_Kind range"
        & ASCII.LF
        & Indent & "  " & To_Ada_Identifier
        (Str => Types.Source_Event_Action_Kind'First'Img)
        & " .. " &  To_Ada_Identifier
        (Str => Types.Source_Event_Action_Kind'Last'Img) & ";"
        & ASCII.LF & ASCII.LF;

      Result := Result & Indent
        & "subtype Target_Event_Action_Kind is Event_Action_Kind range"
        & ASCII.LF
        & Indent & "  " & To_Ada_Identifier
        (Str => Types.Target_Event_Action_Kind'First'Img)
        & " .. " &  To_Ada_Identifier
        (Str => Types.Target_Event_Action_Kind'Last'Img) & ";"
        & ASCII.LF;

      return To_String (Result);
   end Get_Event_Kind_Types_String;

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

      --  Reference: Intel SDM Vol. 3C, "26.3.2.1 Loading Guest Control
      --  Registers, Debug Registers, and MSRs" and Intel SDM Vol. 3C, "27.3.1
      --  Saving Control Registers, Debug Registers, and MSRs".

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

   function Lookup_File
     (Filename    : String;
      Directories : Strings.String_Array)
      return String
   is
   begin
      for Dir of Directories loop
         declare
            Path : constant String
              := Ada.Strings.Unbounded.To_String (Dir) & "/" & Filename;
         begin
            if Ada.Directories.Exists (Name => Path) then
               return Path;
            end if;
         end;
      end loop;

      raise File_Not_Found with "File '" & Filename
        & "' not found in any of the specified directories";
   end Lookup_File;

   -------------------------------------------------------------------------

   function To_Ada_Identifier (Str : String) return String
   is
      Word_Start : Boolean := True;
      Res        : String  := Str;
   begin
      for C of Res loop
         if Ada.Characters.Handling.Is_Alphanumeric (Item => C) then
            C := (if Word_Start then
                     Ada.Characters.Handling.To_Upper (Item => C)
                  else
                     Ada.Characters.Handling.To_Lower (Item => C));
            Word_Start := False;
         else
            C          := '_';
            Word_Start := True;
         end if;
      end loop;

      return Res;
   end To_Ada_Identifier;

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
