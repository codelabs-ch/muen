--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Paging.ARMv8a
is

private

   --  Reference: ARM Arm, D8.3, "Translation table descriptor formats"
   Present_Flag        : constant :=  0;
   Not_Large_Page_Flag : constant :=  1;
   Accessed_Flag       : constant := 10;

   --  Outer shareable, see ARM Arm D8.5.2 "Stage 1 Shareability attributes" and
   --  D8.5.6 "Stage 2 Shareability attributes".
   ARMv8_Outer_Shareable : constant Interfaces.Unsigned_64 := 16#0200#;

end Paging.ARMv8a;
