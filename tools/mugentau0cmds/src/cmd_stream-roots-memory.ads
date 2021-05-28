--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with Interfaces;

with DOM.Core;

with Cmd_Stream.Utils;

package Cmd_Stream.Roots.Memory
is

   --  Generate command stream to create memory regions and their associated
   --  page tables of given system policy.
   procedure Create_Memory_Regions
     (Stream_Doc  : in out Utils.Stream_Document_Type;
      Phys_Memory :        DOM.Core.Node_List);

   Missing_Filesize : exception;

private

   --  Address of next free Tau0 private page.
   Next_Priv_Page : Interfaces.Unsigned_64 := 16#4000_0000_0000#;

end Cmd_Stream.Roots.Memory;
