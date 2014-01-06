--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml;
with Mulog;

with Zp.Generator;

package body Zp
is

   -------------------------------------------------------------------------

   procedure Process
     (Policy     : String;
      Output_Dir : String)
   is
      use McKae.XML.XPath.XIA;

      Zps  : DOM.Core.Node_List;
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Using output directory '" & Output_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy & "'");
      Muxml.Parse (Data => Data,
                   File => Policy);
      Zps := XPath_Query (N     => Data.Doc,
                          XPath => "*/memory/memory/file[@format='zp']");

      for I in 1 .. DOM.Core.Nodes.Length (List => Zps) loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Zps,
                                      Index => I - 1);
            Fn   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
         begin
            Zp.Generator.Write
              (Filename => Output_Dir & "/" & Fn,
               Cmdline  => "lpj=10000 earlyprintk=serial console=hvc0 "
               & "console=ttyS0,115200 pci=noearly notsc");
         end;
      end loop;
   end Process;

end Zp;
