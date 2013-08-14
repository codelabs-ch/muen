--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Skp
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Subject_Type) return Boolean
   is
   begin
      return Left.Id < Right.Id;
   end "<";

   -------------------------------------------------------------------------

   function Get_CPU
     (Subjects   : Subjects_Type;
      Subject_Id : Natural)
      return Integer
   is
      S_Pos : Subjects_Package.Cursor := Subjects.First;
   begin
      while Subjects_Package.Has_Element (Position => S_Pos) loop
         declare
            Subj : constant Subject_Type := Subjects_Package.Element
              (Position => S_Pos);
         begin
            if Subjects_Package.Element
              (Position => S_Pos).Id = Subject_Id
            then
               return Subj.CPU;
            end if;
         end;
         Subjects_Package.Next (Position => S_Pos);
      end loop;

      return -1;
   end Get_CPU;

   -------------------------------------------------------------------------

   function Get_Id
     (Subjects : Subjects_Type;
      Name     : Ada.Strings.Unbounded.Unbounded_String)
      return Integer
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      S_Pos : Subjects_Package.Cursor := Subjects.First;
   begin
      while Subjects_Package.Has_Element (Position => S_Pos) loop
         declare
            Subj : constant Subject_Type := Subjects_Package.Element
              (Position => S_Pos);
         begin
            if Subj.Name = Name then
               return Subj.Id;
            end if;
         end;
         Subjects_Package.Next (Position => S_Pos);
      end loop;

      return -1;
   end Get_Id;

end Skp;
