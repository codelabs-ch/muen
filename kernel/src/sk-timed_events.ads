--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

package SK.Timed_Events
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Get timer information for a subject with given ID.
   procedure Get_Timer
     (Subject :     Skp.Subject_Id_Type;
      Value   : out SK.Word64;
      Vector  : out SK.Byte)
   with
       Global  => (Input => State),
       Depends => ((Value, Vector) => (State, Subject));

   --  Clear timer of subject with given ID.
   procedure Clear_Timer (Subject : Skp.Subject_Id_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject);

   --  Initialize timer of subject with given ID.
   procedure Init_Timer (Subject : Skp.Subject_Id_Type)
   with
      Global  => (In_Out => State),
      Depends => (State => +Subject);

end SK.Timed_Events;
