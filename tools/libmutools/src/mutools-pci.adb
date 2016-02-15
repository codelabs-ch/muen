--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mutools.PCI
is

   -------------------------------------------------------------------------

   function Create
     (Bus    : Bus_Range;
      Device : Device_Range;
      Func   : Function_Range)
      return BDF_Type
   is
   begin
      return BDF_Type'
        (Bus    => Null_Bus_Range (Bus),
         Device => Null_Device_Range (Device),
         Func   => Null_Function_Range (Func));
   end Create;

   -------------------------------------------------------------------------

   function Get_BDF (Dev : DOM.Core.Node) return BDF_Type
   is
      use type DOM.Core.Node;

      PCI_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Dev,
         XPath => "pci");
   begin
      if PCI_Node = null then
         return Null_BDF;
      end if;

      return Create
        (Bus    => Bus_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "bus")),
         Device => Device_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "device")),
         Func   => Function_Range'Value
           (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "function")));
   end Get_BDF;

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

end Mutools.PCI;
