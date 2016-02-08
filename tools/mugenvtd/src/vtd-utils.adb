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

with DOM.Core.Elements;

with Muxml.Utils;

package body VTd.Utils
is

   -------------------------------------------------------------------------

   function Get_BDF (Dev : DOM.Core.Node) return PCI.BDF_Type
   is
      use type DOM.Core.Node;

      PCI_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Dev,
         XPath => "pci");
   begin
      if PCI_Node = null then
         return PCI.Null_BDF;
      end if;

      return PCI.Create
        (Bus    => PCI.Bus_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "bus")),
         Device => PCI.Device_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "device")),
         Func   => PCI.Function_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "function")));
   end Get_BDF;

   -------------------------------------------------------------------------

   procedure Get_IR_TM_SID
     (Kind :     Mutools.XML_Utils.IRQ_Kind;
      BDF  :     PCI.BDF_Type;
      TM   : out Tables.Bit_Type;
      SID  : out Interfaces.Unsigned_16)
   is
   begin
      case Kind is
         when Mutools.XML_Utils.IRQ_ISA =>
            SID := PCI.IOAPIC_Bus_Dev_Func;
            TM  := 0;
         when Mutools.XML_Utils.IRQ_PCI_LSI =>
            SID := PCI.IOAPIC_Bus_Dev_Func;
            TM  := 1;
         when Mutools.XML_Utils.IRQ_PCI_MSI =>
            SID := PCI.To_SID (BDF => BDF);
            TM  := 0;
      end case;
   end Get_IR_TM_SID;

end VTd.Utils;
