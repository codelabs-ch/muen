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

with SK;

with Musinfo;

package Dev_Mngr
is

   type Emul_Req_Op_Type is
     (Emul_Req_Invalid,
      Emul_Req_Read,
      Emul_Req_Write);

   type Offset_Type is new SK.Byte;

   type Emul_Message_Type is record
      SID    : Musinfo.SID_Type;
      Offset : Offset_Type;
      Op     : Emul_Req_Op_Type;
      Value  : SK.Word32;
      Result : SK.Word32;
   end record;

   Null_Emul_Message : constant Emul_Message_Type;

private

   Null_Emul_Message : constant Emul_Message_Type
     := (SID    => 0,
         Offset => 0,
         Op     => Emul_Req_Invalid,
         Value  => 0,
         Result => 0);

end Dev_Mngr;
