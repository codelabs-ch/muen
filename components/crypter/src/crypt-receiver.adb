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

with System;

package body Crypt.Receiver
--# own State is in Request;
is

   Request : Crypt.Message_Type;
   for Request'Address use System'To_Address (16#10000#);
   pragma Volatile (Request);
   --# assert Request'Always_Valid;

   -------------------------------------------------------------------------

   procedure Receive (Req : out Crypt.Message_Type)
   --# global
   --#    in Request;
   --# derives
   --#    Req from Request;
   is
   begin
      Req := Request;
   end Receive;

end Crypt.Receiver;
