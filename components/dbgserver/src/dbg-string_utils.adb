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

package body Dbg.String_Utils
is

   Table : constant String := "0123456789";

   ASCII_0 : constant := Character'Pos ('0');
   ASCII_9 : constant := Character'Pos ('9');
   ASCII_a : constant := Character'Pos ('a');
   ASCII_f : constant := Character'Pos ('f');

   type Digit_Range is range ASCII_0 .. ASCII_9;
   type Hex_Range   is range ASCII_a .. ASCII_f;

   -------------------------------------------------------------------------

   procedure Unsigned_Integer_To_String
     (Unsigned_Number :        Natural;
      Buffer          : in out String;
      Left_Align      :        Boolean := True)
   is
      Number  : Natural := Unsigned_Number;
      Shifter : Natural := Unsigned_Number;
      Pos     : Natural := Buffer'First - 1;
   begin
      if Left_Align then
         loop
            Shifter := Shifter / 10;
            Pos := Pos + 1;
            exit when Shifter = 0;
         end loop;
      else
         Pos := Buffer'Last;
      end if;

      loop
         Buffer (Pos) := Table ((Number mod 10) + Table'First);
         Number := Number / 10;
         Pos := Pos - 1;
         exit when Number = 0;
      end loop;
   end Unsigned_Integer_To_String;

   -------------------------------------------------------------------------

   procedure Address_To_String
     (Address :        Interfaces.Unsigned_64;
      Buffer  : in out String)
   is
      use type Interfaces.Unsigned_64;

      Number  : Interfaces.Unsigned_64 := Address;
      Shifter : Interfaces.Unsigned_64 := Address;
      Pos     : Integer := Buffer'First - 1;
   begin
      loop
         Shifter := Shifter / 16;
         Pos := Pos + 1;
         exit when Shifter = 0;
      end loop;

      loop
         Buffer (Pos) := Table (Integer ((Number mod 16) + 1));
         Number := Number / 16;
         Pos := Pos - 1;
         exit when Number = 0;
      end loop;
   end Address_To_String;

   -------------------------------------------------------------------------

   function Is_Control (Char : Character) return Boolean
   is (Char <= ASCII.US);

   -------------------------------------------------------------------------

   function Is_Digit (Char : Character) return Boolean
   is (Character'Pos (Char) in Digit_Range);

   -------------------------------------------------------------------------

   function Is_Hex (Char : Character) return Boolean
   is (Character'Pos (Char) in Hex_Range);

   -------------------------------------------------------------------------

   function Is_Hex_Digit (Char : Character) return Boolean
   is (Is_Digit (Char => Char) or Is_Hex (Char => Char));

   -------------------------------------------------------------------------

   function Hex_To_Number (Char : Character) return Integer
   is
   begin
      if Is_Digit (Char => Char) then
         return Character'Pos (Char) - ASCII_0;
      elsif Is_Hex (Char => Char) then
         return Character'Pos (Char) - ASCII_a + 10;
      end if;
      return 0;
   end Hex_To_Number;

   -------------------------------------------------------------------------

   procedure Parse_Ada_Hex
     (Text       :     String;
      Text_Start :     Natural;
      Text_End   :     Natural;
      Number     : out Natural)
   is
   begin
      Number := 0;
      for I in Text_Start .. Text_End loop
         Number := Number * 16 + Hex_To_Number (Text (I));
      end loop;
   end Parse_Ada_Hex;

   -------------------------------------------------------------------------

   procedure Consume_Ada_Hex
     (Text       :        String;
      Text_Start : in out Natural;
      Text_End   :        Natural;
      Found      :    out Boolean;
      Number     :    out Natural)
   is
      End_Of_Digits : Natural;
   begin
      Found := False;

      for I in Text_Start + 3 .. Text_End loop
         exit when not Is_Hex_Digit (Text (I));
         End_Of_Digits := I;
         Found := True;
      end loop;

      if Found then
         Parse_Ada_Hex (Text, Text_Start + 3, End_Of_Digits, Number);
         Text_Start := End_Of_Digits + 1;
      else
         Number := 0;
      end if;
   end Consume_Ada_Hex;

   -------------------------------------------------------------------------

   procedure Parse_Integer
     (Text       :     String;
      Text_Start :     Natural;
      Text_End   :     Natural;
      Number     : out Natural)
   is
   begin
      Number := 0;

      for I in Text_Start .. Text_End loop
         Number := Number * 10 + (Character'Pos (Text (I)) - ASCII_0);
      end loop;
   end Parse_Integer;

   -------------------------------------------------------------------------

   procedure Consume_Integer
     (Text       :        String;
      Text_Start : in out Natural;
      Text_End   :        Natural;
      Number     :    out Natural)
   is
      End_Of_Digits : Natural;
      Found         : Boolean := False;
   begin
      for I in Text_Start .. Text_End loop
         exit when not Is_Digit (Text (I));
         End_Of_Digits := I;
         Found := True;
      end loop;

      if Found then
         Parse_Integer (Text, Text_Start, End_Of_Digits, Number);
         Text_Start := End_Of_Digits + 1;
      else
         Number := 0;
      end if;
   end Consume_Integer;

   -------------------------------------------------------------------------

   function Starts_With
     (Str    : String;
      Prefix : String)
      return Boolean
   is
   begin
      if Prefix'Length > Str'Length then
         return False;
      else
         for I in Prefix'First .. Prefix'Last loop
            if Prefix (I) /= Str (I) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Starts_With;

   -------------------------------------------------------------------------

   function Index
     (Source : String;
      From   : Natural;
      Char   : Character)
      return Natural
   is
   begin
      for I in From .. Source'Last loop
         if Source (I) = Char then
            return I;
         end if;
      end loop;
      return Source'Last + 1;
   end Index;

end Dbg.String_Utils;
