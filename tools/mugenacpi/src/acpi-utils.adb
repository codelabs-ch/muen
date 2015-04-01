--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Acpi.Utils
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Add_Dev_IRQ_Resource
     (Buffer  : in out Unbounded_String;
      Bus_Nr  :        Interfaces.Unsigned_64;
      Dev_Nr  :        Interfaces.Unsigned_64;
      Irq_Nr  :        Interfaces.Unsigned_64;
      Int_Pin :        Natural)
   is
   begin
      Buffer := Buffer & Indent (N => 5) & "Package (4) { 0x";
      Buffer := Buffer & Mutools.Utils.To_Hex
        (Number     => Bus_Nr,
         Normalize  => False,
         Byte_Short => True);
      Buffer := Buffer & Mutools.Utils.To_Hex
        (Number     => Dev_Nr,
         Normalize  => False,
         Byte_Short => True);
      Buffer := Buffer & "ffff,";
      Buffer := Buffer & Int_Pin'Img & ", Zero, 0x";
      Buffer := Buffer & Mutools.Utils.To_Hex
        (Number    => Irq_Nr,
         Normalize => False) & " }," & ASCII.LF;
   end Add_Dev_IRQ_Resource;

end Acpi.Utils;
