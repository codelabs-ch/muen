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

with X86_64;

package Foo.Sender
with
   Abstract_State => (State with External => Async_Readers),
   Initializes    => State
is

   --  Copies the given response message into the response page.
   procedure Send (Res : Foo.Message_Type)
   with
      Global  => (Output => State, In_Out => X86_64.State),
      Depends => (State => Res, X86_64.State =>+ null);

end Foo.Sender;
