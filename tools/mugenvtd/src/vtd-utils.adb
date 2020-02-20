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

package body VTd.Utils
is

   -------------------------------------------------------------------------

   procedure Get_IR_TM_SID
     (Kind       :     Mutools.XML_Utils.IRQ_Kind;
      BDF        :     Mutools.PCI.BDF_Type;
      IOAPIC_SID :     String;
      TM         : out Tables.Bit_Type;
      SID        : out Interfaces.Unsigned_16)
   is
   begin
      case Kind is
         when Mutools.XML_Utils.IRQ_ISA =>
            SID := Interfaces.Unsigned_16'Value (IOAPIC_SID);
            TM  := 0;
         when Mutools.XML_Utils.IRQ_PCI_LSI =>
            SID := Interfaces.Unsigned_16'Value (IOAPIC_SID);
            TM  := 1;
         when Mutools.XML_Utils.IRQ_PCI_MSI =>
            SID := Mutools.PCI.To_SID (BDF => BDF);
            TM  := 0;
      end case;
   end Get_IR_TM_SID;

end VTd.Utils;
