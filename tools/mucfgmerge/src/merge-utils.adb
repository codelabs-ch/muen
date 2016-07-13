--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Merge.Utils
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function "&"
     (Arr : String_Array;
      Str : String)
      return String_Array
   is
   begin
      return New_Array : String_Array (Arr'First .. Arr'Last + 1) do
         New_Array (Arr'Range) := Arr;
         New_Array (New_Array'Last) := U (Str);
      end return;
   end "&";

   -------------------------------------------------------------------------

   function Lookup_File
     (Filename    : String;
      Directories : String_Array)
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

   function Tokenize
     (Str       : String;
      Separator : Character := ':')
      return String_Array
   is
      Nr_Tokens : constant Positive
        := Ada.Strings.Fixed.Count
          (Source  => Str,
           Pattern => (1 => Separator)) + 1;

      Result  : String_Array (1 .. Nr_Tokens);
      Cur_Idx : Natural := Str'First;
   begin
      for T of Result loop
         declare
            Next_Idx : constant Natural
              := Ada.Strings.Fixed.Index
                (Source  => Str,
                 Pattern => (1 => Separator),
                 From    => Cur_Idx);
            End_Idx  : constant Natural
              := (if Next_Idx /= 0 then Next_Idx - 1 else Str'Last);
         begin
            T := U (Str (Cur_Idx .. End_Idx));
            Cur_Idx := Next_Idx + 1;
         end;
      end loop;

      return Result;
   end Tokenize;

end Merge.Utils;
