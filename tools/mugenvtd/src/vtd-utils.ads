--
--  Copyright (C) 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutools.PCI;
with Mutools.XML_Utils;

with VTd.Tables;

package VTd.Utils
is

   --  Return IR trigger mode and source-identifier for given IRQ kind, PCI BDF
   --  triplet and I/O APIC source ID.
   procedure Get_IR_TM_SID
     (Kind       :     Mutools.XML_Utils.IRQ_Kind;
      BDF        :     Mutools.PCI.BDF_Type;
      IOAPIC_SID :     String;
      TM         : out Tables.Bit_Type;
      SID        : out Interfaces.Unsigned_16);

end VTd.Utils;
