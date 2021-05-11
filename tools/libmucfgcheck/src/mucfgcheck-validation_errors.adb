--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mucfgcheck.Validation_Errors
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Check
   is
   begin
      if not Is_Empty then
         raise Validation_Error;
      end if;
   end Check;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Errors.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Contains (Msg : String) return Boolean
   is
      use type List_Of_Ustring_Pkg.Cursor;
   begin
      if Errors.Find (To_Unbounded_String (Msg))
        /= List_Of_Ustring_Pkg.No_Element
      then
         return True;
      end if;

      return False;
   end Contains;

   -------------------------------------------------------------------------

   function Get_Error_Message return String
   is
      Nr  : Positive := Positive'First;
      Res : Unbounded_String;
   begin
      Res := To_Unbounded_String
        ("Found" & Errors.Length'Img & " error(s):"
         & ASCII.LF & ASCII.LF);

      for E of Errors loop
         Res := Res & Nr'Img & ": " & E & ASCII.LF;
         Nr := Nr + 1;
      end loop;

      return To_String (Res);
   end Get_Error_Message;

   -------------------------------------------------------------------------

   procedure Insert
     (Msg   : String;
      Fatal : Boolean := False)
   is
   begin
      Errors.Append (New_Item => To_Unbounded_String (Msg));

      if Fatal then
         raise Validation_Error;
      end if;
   end Insert;

end Mucfgcheck.Validation_Errors;
