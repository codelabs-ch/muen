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

with Mucfgcheck.Validation_Errors;

package body Mucfgcheck.MSR
is

   use McKae.XML.XPath.XIA;

   package MC renames Mutools.Constants;

   -------------------------------------------------------------------------

   procedure Check_Whitelist (XML_Data : Muxml.XML_Data_Type)
   is
      type MSR_Whitelist_Entry_Type is record
         Start_Address : Interfaces.Unsigned_32;
         End_Address   : Interfaces.Unsigned_32;
      end record;

      MSR_Whitelist : constant array (Natural range <>)
        of MSR_Whitelist_Entry_Type
          := (1  => (Start_Address => MC.IA32_SYSENTER_CS,
                     End_Address   => MC.IA32_SYSENTER_EIP),
              2  => (Start_Address => MC.IA32_DEBUGCTL,
                     End_Address   => MC.IA32_DEBUGCTL),
              3  => (Start_Address => MC.IA32_EFER,
                     End_Address   => MC.IA32_FMASK),
              4  => (Start_Address => MC.IA32_FS_BASE,
                     End_Address   => MC.IA32_KERNEL_GS_BASE),
              5  => (Start_Address => MC.MSR_PLATFORM_INFO,
                     End_Address   => MC.MSR_PLATFORM_INFO),
              6  => (Start_Address => MC.IA32_THERM_STATUS,
                     End_Address   => MC.IA32_THERM_STATUS),
              7  => (Start_Address => MC.IA32_TEMPERATURE_TARGET,
                     End_Address   => MC.IA32_TEMPERATURE_TARGET),
              8  => (Start_Address => MC.IA32_PACKAGE_THERM_STATUS,
                     End_Address   => MC.IA32_PACKAGE_THERM_STATUS),
              9  => (Start_Address => MC.MSR_RAPL_POWER_UNIT,
                     End_Address   => MC.MSR_RAPL_POWER_UNIT),
              10 => (Start_Address => MC.MSR_PKG_POWER_LIMIT,
                     End_Address   => MC.MSR_PKG_ENERGY_STATUS),
              11 => (Start_Address => MC.MSR_DRAM_ENERGY_STATUS,
                     End_Address   => MC.MSR_DRAM_ENERGY_STATUS),
              12 => (Start_Address => MC.MSR_PP1_ENERGY_STATUS,
                     End_Address   => MC.MSR_PP1_ENERGY_STATUS),
              13 => (Start_Address => MC.MSR_CONFIG_TDP_CONTROL,
                     End_Address   => MC.MSR_CONFIG_TDP_CONTROL),
              14 => (Start_Address => MC.IA32_PM_ENABLE,
                     End_Address   => MC.IA32_HWP_CAPABILITIES),
              15 => (Start_Address => MC.IA32_HWP_REQUEST,
                     End_Address   => MC.IA32_HWP_REQUEST));

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//msrs/msr");
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
            Valid_Range : Boolean := False;
         begin
            Check_MSR_Range :
            for E of MSR_Whitelist loop
               if S_Addr in E.Start_Address .. E.End_Address
                 and E_Addr in E.Start_Address .. E.End_Address
               then
                  Valid_Range := True;
                  exit Check_MSR_Range;
               end if;
            end loop Check_MSR_Range;

            if not Valid_Range then
               Validation_Errors.Insert
                 (Msg => "MSR start " & S_Addr_Str
                  & " and end " & E_Addr_Str & " not in MSR whitelist"
                  & " (Subject '" & Subj_Name & "')");
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
         XPath => "//msrs/msr");
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
               Validation_Errors.Insert
                 (Msg => "MSR start " & S_Addr_Str
                  & " larger than end " & E_Addr_Str & " (Subject '" & Name
                  & "')");
            end if;
         end;
      end loop;
   end Start_Smaller_End;

end Mucfgcheck.MSR;
