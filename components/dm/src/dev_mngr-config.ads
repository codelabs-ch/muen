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

package Dev_Mngr.Config
is

   use type SK.Word64;

   MMConf_Base_Address : constant SK.Word64 := 16#f800_0000#;
   MMConf_Size         : constant SK.Word64 := 16#1000_0000#;

   subtype MMConf_Region is SK.Word64 range
     MMConf_Base_Address .. MMConf_Base_Address + MMConf_Size - 1;

end Dev_Mngr.Config;
