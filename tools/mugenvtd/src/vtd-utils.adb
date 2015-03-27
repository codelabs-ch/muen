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

package body VTd.Utils
is

   -------------------------------------------------------------------------

   procedure Get_IR_TM_SID
     (Kind :     Mutools.XML_Utils.IRQ_Kind;
      TM   : out Tables.Bit_Type;
      SID  : out Interfaces.Unsigned_16)
   is
   begin
      SID := IOAPIC_Bus_Dev_Func;

      case Kind is
         when Mutools.XML_Utils.IRQ_ISA =>
            TM := 0;
         when Mutools.XML_Utils.IRQ_PCI_LSI | Mutools.XML_Utils.IRQ_PCI_MSI =>
            TM := 1;
      end case;
   end Get_IR_TM_SID;

   -------------------------------------------------------------------------

   function To_SID (BDF : BDF_Type) return Interfaces.Unsigned_16
   is
      use type Interfaces.Unsigned_16;

      SID : Interfaces.Unsigned_16 := Interfaces.Unsigned_16 (BDF.Func);
   begin
      SID := SID or Interfaces.Unsigned_16 (BDF.Device) * 2 ** 3;
      SID := SID or Interfaces.Unsigned_16 (BDF.Bus)    * 2 ** 8;

      return SID;
   end To_SID;

end VTd.Utils;
