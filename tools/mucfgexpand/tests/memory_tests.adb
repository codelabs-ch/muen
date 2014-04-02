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

with Expanders.Memory;

with Test_Utils.Expander;

package body Memory_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Add_AP_Trampoline
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_trampoline.xml",
         Ref_Filename => "data/memory_trampoline.ref.xml",
         Expander     => Expanders.Memory.Add_AP_Trampoline'Access);
   end Add_AP_Trampoline;

   -------------------------------------------------------------------------

   procedure Add_Binary_Memory
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_kernel_binary.xml",
         Ref_Filename => "data/memory_kernel_binary.ref.xml",
         Expander     => Expanders.Memory.Add_Kernel_Binary'Access);
   end Add_Binary_Memory;

   -------------------------------------------------------------------------

   procedure Add_Stack_Store
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_stack_store.xml",
         Ref_Filename => "data/memory_stack_store.ref.xml",
         Expander     => Expanders.Memory.Add_Stack_Store'Access);
   end Add_Stack_Store;

   -------------------------------------------------------------------------

   procedure Add_Subject_States
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_subject_states.xml",
         Ref_Filename => "data/memory_subject_states.ref.xml",
         Expander     => Expanders.Memory.Add_Subject_States'Access);
   end Add_Subject_States;

   -------------------------------------------------------------------------

   procedure Add_Tau0_Interface
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_tau0_iface.xml",
         Ref_Filename => "data/memory_tau0_iface.ref.xml",
         Expander     => Expanders.Memory.Add_Tau0_Interface'Access);
   end Add_Tau0_Interface;

   -------------------------------------------------------------------------

   procedure Add_VMXON_Regions
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_vmxon.xml",
         Ref_Filename => "data/memory_vmxon.ref.xml",
         Expander     => Expanders.Memory.Add_VMXON_Regions'Access);
   end Add_VMXON_Regions;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Memory expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binary_Memory'Access,
         Name    => "Add kernel binary memory regions");
      T.Add_Test_Routine
        (Routine => Add_Stack_Store'Access,
         Name    => "Add kernel stack and store memory regions");
      T.Add_Test_Routine
        (Routine => Add_Subject_States'Access,
         Name    => "Add subject state memory regions");
      T.Add_Test_Routine
        (Routine => Add_Tau0_Interface'Access,
         Name    => "Add tau0 interface memory region");
      T.Add_Test_Routine
        (Routine => Add_AP_Trampoline'Access,
         Name    => "Add AP trampoline memory region");
      T.Add_Test_Routine
        (Routine => Add_VMXON_Regions'Access,
         Name    => "Add VMXON memory regions");
   end Initialize;

end Memory_Tests;
