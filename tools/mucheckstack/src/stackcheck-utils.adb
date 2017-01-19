--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with Mutools.Utils;

package body Stackcheck.Utils
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Entity_To_Ada_Name (Str : String) return String
   is
      --  Strip * at beginning of string if present.
      Start : constant Natural
        := (if Str'Length > 0 and then Str (Str'First) = '*'
            then Str'First + 1 else Str'First);

      Tmp : Unbounded_String := To_Unbounded_String
        (Mutools.Utils.To_Ada_Identifier (Str (Start .. Str'Last)));
      Pos : Natural := Start;
   begin
      loop
         Pos := Index (Source  => Tmp,
                       Pattern => "__",
                       From    => Pos);
         exit when Pos = 0;
         Replace_Slice (Source => Tmp,
                        Low    => Pos,
                        High   => Pos + 1,
                        By     => ".");
         Pos := Pos + 1;
      end loop;

      --  Handle elaboration names.

      if Tail (Source => Tmp,
               Count  => 7) = "._Elabs"
      then
         Ada.Strings.Unbounded.
           Replace_Slice (Source => Tmp,
                          Low    => Length (Tmp) - 6,
                          High   => Length (Tmp),
                          By     => "'Elab_Spec");
      elsif Tail (Source => Tmp,
                  Count  => 7) = "._Elabb"
      then
         Ada.Strings.Unbounded.
           Replace_Slice (Source => Tmp,
                          Low    => Length (Tmp) - 6,
                          High   => Length (Tmp),
                          By     => "'Elab_Body");
      end if;

      return To_String (Tmp);
   end Entity_To_Ada_Name;

end Stackcheck.Utils;
