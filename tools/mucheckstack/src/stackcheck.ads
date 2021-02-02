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

package Stackcheck
is

   --  Start the worst-case stack usage check process with specified limit
   --  using given GNAT project file. Overflow is set to True if any stack
   --  usage above the given limit is detected. Dynamic is set to True if any
   --  dynamic unbounded stack usage is detected.
   --
   --  Note: A limit of zero means that no stack usage is allowed.
   procedure Run
     (Project_File :     String;
      Limit        :     Natural;
      Overflow     : out Boolean;
      Dynamic      : out Boolean);

   Process_Error : exception;

end Stackcheck;
