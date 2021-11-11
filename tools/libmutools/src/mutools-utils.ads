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

with Interfaces;

with Mutools.Strings;

package Mutools.Utils
is

   type Unsigned_64_Pos is range 0 .. 63;

   --  Test if bit at given position is set.
   function Bit_Test
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Boolean;

   --  Set bit at given position.
   function Bit_Set
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64;

   --  Clear bit at given position.
   function Bit_Clear
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64;

   --  Return hexadecimal representation of given number. If Normalize is True,
   --  the returned string includes the base (16#..#) and consist of blocks of
   --  0000 separated by '_'. If Byte_Short is True, numbers smaller or equal
   --  255 are represented by blocks of 00.
   function To_Hex
     (Number     : Interfaces.Unsigned_64;
      Normalize  : Boolean := True;
      Byte_Short : Boolean := False)
      return String;

   --  Return N number of indentation units. The unit size specifies the number
   --  of spaces per unit.
   function Indent
     (N         : Positive := 1;
      Unit_Size : Positive := 3)
      return String;

   --  Return capitalised version of the given string (first letter in
   --  uppercase and the remaining letters remain as-is).
   function Capitalize (Str : String) return String;

   --  Convert given string to Ada Mixed_Case_With_Underscores format.
   function To_Ada_Identifier (Str : String) return String;

   --  Extract entity name from given encoded string (e.g. 'linux|zp' or
   --  'kernel_0|vmxon').
   function Decode_Entity_Name (Encoded_Str : String) return String;

   --  Returns True if the given MSR is automatically stored/loaded to/from the
   --  VMCS guest state area on VM-exit/entry also considering the given
   --  VM-controls.
   function Is_Managed_By_VMX
     (MSR                    : Interfaces.Unsigned_64;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
      return Boolean;

   --  Returns True if the given MSR is a global/system-wide MSR and must not be
   --  saved/restored.
   function Is_Global_MSR (MSR : Interfaces.Unsigned_64) return Boolean;

   --  Searches the specified directories and returns the full path to the
   --  file with given name. An exception is raised if none of the specified
   --  directories contains such a file.
   function Lookup_File
     (Filename    : String;
      Directories : Strings.String_Array)
      return String;

   --  Returns the event action kinds as Ada type definition string.
   function Get_Event_Kind_Types_String return String;

   File_Not_Found : exception;

end Mutools.Utils;
