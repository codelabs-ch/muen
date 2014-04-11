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

with Muxml;

with Expanders.Kernel;
with Expanders.Subjects;

with Test_Utils.Expander;

package body Kernel_Tests
is

   use Ahven;

   --  Invoke kernel section skeleton and subject id expanders.
   procedure Pre_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Add_Binary_Mappings
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/kernel_binary_mappings.xml",
         Ref_Filename => "data/kernel_binary_mappings.ref.xml",
         Pre          => Expanders.Kernel.Add_Section_Skeleton'Access,
         Expander     => Expanders.Kernel.Add_Binary_Mappings'Access);
   end Add_Binary_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Devices
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/kernel_devices.xml",
         Ref_Filename => "data/kernel_devices.ref.xml",
         Pre          => Expanders.Kernel.Add_Section_Skeleton'Access,
         Expander     => Expanders.Kernel.Add_Devices'Access);
   end Add_Devices;

   -------------------------------------------------------------------------

   procedure Add_Subj_State_Mappings
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/kernel_subj_state_mappings.xml",
         Ref_Filename => "data/kernel_subj_state_mappings.ref.xml",
         Pre          => Pre_Subj_State_Mappings'Access,
         Expander     => Expanders.Kernel.Add_Subj_State_Mappings'Access);
   end Add_Subj_State_Mappings;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Kernel expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binary_Mappings'Access,
         Name    => "Add binary mappings");
      T.Add_Test_Routine
        (Routine => Add_Subj_State_Mappings'Access,
         Name    => "Add subject state mappings");
      T.Add_Test_Routine
        (Routine => Map_Tau0_Interface'Access,
         Name    => "Map tau0 interface memory region");
      T.Add_Test_Routine
        (Routine => Add_Devices'Access,
         Name    => "Add kernel devices");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Map_Tau0_Interface
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/kernel_tau0_iface_mapping.xml",
         Ref_Filename => "data/kernel_tau0_iface_mapping.ref.xml",
         Pre          => Expanders.Kernel.Add_Section_Skeleton'Access,
         Expander     => Expanders.Kernel.Map_Tau0_Interface'Access);
   end Map_Tau0_Interface;

   -------------------------------------------------------------------------

   procedure Pre_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Expanders.Kernel.Add_Section_Skeleton (Data => Data);
      Expanders.Subjects.Add_Ids (Data => Data);
   end Pre_Subj_State_Mappings;

end Kernel_Tests;
