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

--# inherit
--#    X86_64,
--#    SK;
package SK.IO
is

   --  Receive byte from given port.
   procedure Inb
     (Port  :     SK.Word16;
      Value : out SK.Byte);
   --# global
   --#    in X86_64.State;
   --# derives
   --#    Value from Port, X86_64.State;

   --  Send byte to given port.
   procedure Outb
     (Port  : SK.Word16;
      Value : SK.Byte);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Port, Value;

   --  Send 16-bit word to given port.
   procedure Outw
     (Port  : SK.Word16;
      Value : SK.Word16);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Port, Value;

end SK.IO;
