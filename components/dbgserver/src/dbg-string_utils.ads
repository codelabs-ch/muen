--
--  Copyright (C) 2017  secunet Security Networks AG
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Dbg.String_Utils
is

   --  Writes an unsigned integer to a string buffer. If Left_Align is False,
   --  then the number is right aligned.
   procedure Unsigned_Integer_To_String
     (Unsigned_Number :        Natural;
      Buffer          : in out String;
      Left_Align      :        Boolean := True);

   --  Writes an address to a string buffer, right-aligned.
   procedure Address_To_String
     (Address :        Interfaces.Unsigned_64;
      Buffer  : in out String);

   --  Returns True, if the given character denotes a digit.
   function Is_Digit (Char : Character) return Boolean;

   --  Returns True, if the given character denotes a hex digit.
   function Is_Hex_Digit (Char : Character) return Boolean;

   --  Returns True, if the given character is a control/non-printable char.
   function Is_Control (Char : Character) return Boolean;

   --  Returns a number for a given character.
   function Hex_To_Number (Char : Character) return Integer;

   --  Parses an Ada_Hex from a String.
   procedure Parse_Ada_Hex
     (Text       :     String;
      Text_Start :     Natural;
      Text_End   :     Natural;
      Number     : out Natural);

   --  Parses an Ada_Hex from a String and advances Text_Start to the
   --  next character that does not belong to the parsed integer.
   procedure Consume_Ada_Hex
     (Text       :        String;
      Text_Start : in out Natural;
      Text_End   :        Natural;
      Found      :    out Boolean;
      Number     :    out Natural);

   --  Parses an unsigned integer from a String.
   procedure Parse_Integer
     (Text       :     String;
      Text_Start :     Natural;
      Text_End   :     Natural;
      Number     : out Natural);

   --  Parses an unsigned Integer from a String and advances Text_Start to the
   --  next character that does not belong to the parsed integer.
   procedure Consume_Integer
     (Text       :        String;
      Text_Start : in out Natural;
      Text_End   :        Natural;
      Number     :    out Natural);

   --  Returns True, if the given String Str starts with the specified Prefix.
   function Starts_With
     (Str    : String;
      Prefix : String)
      return Boolean;

   --  Returns the Index of the specified character in the given Source
   --  starting at position From. If the character is not present in the source
   --  then Source'Last + 1 is returned.
   function Index
     (Source : String;
      From   : Natural;
      Char   : Character)
      return Natural;

end Dbg.String_Utils;
