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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;

package body Mucfgcheck.MSR
is

   use McKae.XML.XPath.XIA;

   package MC renames Mutools.Constants;

   -------------------------------------------------------------------------

   procedure Check_Whitelist (XML_Data : Muxml.XML_Data_Type)
   is
      subtype MSR_Whitelist_1 is Interfaces.Unsigned_32 range
        MC.IA32_SYSENTER_CS .. MC.IA32_SYSENTER_EIP;
      subtype MSR_Whitelist_2 is Interfaces.Unsigned_32 range
        MC.IA32_DEBUGCTL .. MC.IA32_DEBUGCTL;
      subtype MSR_Whitelist_3 is Interfaces.Unsigned_32 range
        MC.IA32_EFER .. MC.IA32_FMASK;
      subtype MSR_Whitelist_4 is Interfaces.Unsigned_32 range
        MC.IA32_FS_BASE .. MC.IA32_KERNEL_GS_BASE;

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//msr");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " MSR range(s) against whitelist");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Node,
                    Level => 3),
                 Name => "name");
            S_Addr_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "start");
            S_Addr : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (S_Addr_Str);
            E_Addr_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "end");
            E_Addr : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (E_Addr_Str);
         begin
            if not
              ((S_Addr in MSR_Whitelist_1 and E_Addr in MSR_Whitelist_1)
               or (S_Addr in MSR_Whitelist_2 and E_Addr in MSR_Whitelist_2)
               or (S_Addr in MSR_Whitelist_3 and E_Addr in MSR_Whitelist_3)
               or (S_Addr in MSR_Whitelist_4 and E_Addr in MSR_Whitelist_4))
            then
               raise Validation_Error with "MSR start " & S_Addr_Str
                 & " and end " & E_Addr_Str & " not in MSR whitelist"
                 & " (Subject '" & Subj_Name & "')";

            end if;
         end;
      end loop;
   end Check_Whitelist;

   -------------------------------------------------------------------------

   procedure Start_Smaller_End (XML_Data : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_32;

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//msr");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " MSR range(s) for start <= end");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Name     : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Muxml.Utils.Ancestor_Node
                 (Node  => Node,
                  Level => 3),
               Name => "name");
            S_Addr_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "start");
            S_Addr : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (S_Addr_Str);
            E_Addr_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "end");
            E_Addr : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value (E_Addr_Str);
         begin
            if S_Addr > E_Addr then
               raise Validation_Error with "MSR start " & S_Addr_Str
                 & " larger than end " & E_Addr_Str & " (Subject '" & Name
                 & "')";
            end if;
         end;
      end loop;
   end Start_Smaller_End;

end Mucfgcheck.MSR;
