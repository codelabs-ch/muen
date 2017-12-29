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

package Ahci.Constants
is

   AHCI_Class_Code : constant := 16#010601#;

   --  Serial ATA AHCI 1.3.1 Specification, section 3.1.1.
   Interface_Speed_Gen_1 : constant := 2#0001#;
   Interface_Speed_Gen_2 : constant := 2#0010#;
   Interface_Speed_Gen_3 : constant := 2#0011#;

end Ahci.Constants;
