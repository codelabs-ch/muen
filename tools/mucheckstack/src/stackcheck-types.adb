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

package body Stackcheck.Types
is

   -------------------------------------------------------------------------

   procedure Add_Call
     (Subprogram  : in out Subprogram_Type;
      Callee_Name :        String)
   is
   begin
      Subprogram.Calls.Append
        (New_Item => To_Unbounded_String (Source => Callee_Name));
   end Add_Call;

   -------------------------------------------------------------------------

   function Get_Call_Count (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Natural (Subprogram.Calls.Length);
   end Get_Call_Count;

   -------------------------------------------------------------------------

   function Get_Max_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Max_Stack_Usage;
   end Get_Max_Stack_Usage;

   -------------------------------------------------------------------------

   function Get_Name (Subprogram : Subprogram_Type) return String
   is
   begin
      return To_String (Subprogram.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Own_Stack_Usage;
   end Get_Stack_Usage;

   -------------------------------------------------------------------------

   procedure Iterate_Calls
     (Subprogram : Subprogram_Type;
      Process    : not null access procedure (Callee : String))
   is

      --  Call the process procedure for the element designated by the given
      --  cursor.
      procedure Call_Process (Cursor : LOSC.Cursor);

      ----------------------------------------------------------------------

      procedure Call_Process (Cursor : LOSC.Cursor)
      is
      begin
         Process (Callee => To_String (LOSC.Element (Position => Cursor)));
      end Call_Process;

   begin
      Subprogram.Calls.Iterate (Process => Call_Process'Access);
   end Iterate_Calls;

end Stackcheck.Types;
