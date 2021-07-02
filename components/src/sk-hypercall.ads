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

--D @Section Id => interfaces_hypercall, Label => Hypercalls, Parent => interface_section, Priority => 0
--D @Text Section => interfaces_hypercall, Priority => 0
--D The SK.Hypercall package is used to trigger hypercalls into the Muen SK.
--D Components are only able to trigger events which are defined in the system
--D policy.
package SK.Hypercall
is

   --D @Text Section => interfaces_hypercall, Priority => 10
   --D The Trigger\_Event procedure triggers a hypercall given by the Number
   --D argument.
   procedure Trigger_Event (Number : SK.Byte)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Number);

end SK.Hypercall;
