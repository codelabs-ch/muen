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

with Interfaces;

with Mutools.XML_Utils;

with VTd.Tables;

package VTd.Utils
is

   --  Return IR trigger mode and source-identifier for given IRQ kind.
   procedure Get_IR_TM_SID
     (Kind :     Mutools.XML_Utils.IRQ_Kind;
      TM   : out Tables.Bit_Type;
      SID  : out Interfaces.Unsigned_16);

   --  Convert given PCI BDF to IR SID.
   function To_SID (BDF : BDF_Type) return Interfaces.Unsigned_16;

private

   --  I/O APIC bus(8)/dev(5)/func(3) f0:1f.00
   IOAPIC_Bus_Dev_Func : constant := 16#f0f8#;

end VTd.Utils;
