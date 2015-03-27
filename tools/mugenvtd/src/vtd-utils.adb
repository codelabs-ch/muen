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

with DOM.Core.Elements;

with Muxml.Utils;

package body VTd.Utils
is

   -------------------------------------------------------------------------

   function Get_BDF (Dev : DOM.Core.Node) return BDF_Type
   is
      use type DOM.Core.Node;

      PCI : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Dev,
         XPath => "pci");
   begin
      if PCI = null then
         return Null_BDF;
      end if;

      return BDF : BDF_Type do
         BDF.Bus    := Null_Bus_Range'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => PCI,
               Name => "bus"));
         BDF.Device := Null_Device_Range'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => PCI,
               Name => "device"));
         BDF.Func   := Null_Function_Range'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => PCI,
               Name => "function"));
      end return;
   end Get_BDF;

   -------------------------------------------------------------------------

   procedure Get_IR_TM_SID
     (Kind :     Mutools.XML_Utils.IRQ_Kind;
      BDF  :     BDF_Type;
      TM   : out Tables.Bit_Type;
      SID  : out Interfaces.Unsigned_16)
   is
   begin
      case Kind is
         when Mutools.XML_Utils.IRQ_ISA =>
            SID := IOAPIC_Bus_Dev_Func;
            TM  := 0;
         when Mutools.XML_Utils.IRQ_PCI_LSI =>
            SID := IOAPIC_Bus_Dev_Func;
            TM  := 1;
         when Mutools.XML_Utils.IRQ_PCI_MSI =>
            SID := To_SID (BDF => BDF);
            TM  := 0;
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
