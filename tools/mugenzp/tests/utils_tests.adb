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

with DOM.Core;

with McKae.XML.XPath.XIA;

with Muxml;

with bootparam_h;

with Zp.Utils;
with Zp.Constants;

package body Utils_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Create_e820_Map
   is
      use type bootparam_h.boot_params_e820_map_array;

      Ref_Map : constant bootparam_h.boot_params_e820_map_array
        := (0      => (addr   => 16#3000#,
                       size   => 16#d000#,
                       c_type => Zp.Constants.E820_RAM),
            1      => (addr   => 16#1000#,
                       size   => 16#1000#,
                       c_type => Zp.Constants.E820_RAM),
            2      => (addr   => 16#f000#,
                       size   => 16#9000#,
                       c_type => Zp.Constants.E820_RAM),
            3      => (addr   => 16#1000_0000#,
                       size   => 16#1000#,
                       c_type => Zp.Constants.E820_ACPI),
            4      => (addr   => 16#ffff_f000#,
                       size   => 16#1000#,
                       c_type => Zp.Constants.E820_RESERVED),
            others => (addr   => 0,
                       size   => 0,
                       c_type => 0));
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Mem_Nodes : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/subjects/subject[@name='lnx']/memory/memory");
         Map : bootparam_h.boot_params_e820_map_array;
      begin
         Map := Zp.Utils.Create_e820_Map (Memory => Mem_Nodes);

         Assert (Condition => Ref_Map = Map,
                 Message   => "e820 map differs");
      end;
   end Create_e820_Map;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Utils tests");
      T.Add_Test_Routine
        (Routine => Create_e820_Map'Access,
         Name    => "Create e820 map");
   end Initialize;

end Utils_Tests;
