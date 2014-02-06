--
--  Copyright (C) 2013, 2014  Alexander Senier <mail@senier.net>
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
with Alloc.Allocator;
with Test_Utils;

package body Allocation_Tests
is
   use Ahven;
   use Alloc;
   use Test_Utils;

   -------------------------------------------------------------------------

   procedure Allocation_With_Devices
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/allocation_with_devices.in.xml");

      Allocator.Write (Output_Dir => "obj",
                       Policy      => Policy);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/allocation_with_devices.ref.xml",
                   Filename2 => "obj/system.xml"),
              Message => "Invalid allocation involving devices");
   end Allocation_With_Devices;

   -------------------------------------------------------------------------

   procedure Automatic_Allocation
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/automatic_allocation.in.xml");

      Allocator.Write (Output_Dir => "obj",
                       Policy      => Policy);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/automatic_allocation.ref.xml",
                   Filename2 => "obj/system.xml"),
              Message => "Automatic allocation");
   end Automatic_Allocation;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Allocation tests");
      T.Add_Test_Routine
        (Routine => Overlapping_Physical_Memory'Access,
         Name    => "Overlap detection");
      T.Add_Test_Routine
        (Routine => Automatic_Allocation'Access,
         Name    => "Automatic allocation");
      T.Add_Test_Routine
        (Routine => Limited_Allocation'Access,
         Name    => "Limited allocation");
      T.Add_Test_Routine
        (Routine => Allocation_With_Devices'Access,
         Name    => "Allocation with devices");
      T.Add_Test_Routine
        (Routine => Overlapping_Devices'Access,
         Name    => "Overlap between device and RAM");
      T.Add_Test_Routine
        (Routine => Overlap_Between_Devices'Access,
         Name    => "Overlap between different devices");
      T.Add_Test_Routine
        (Routine => Overlap_Between_Device_Memory'Access,
         Name    => "Overlap between memory of one device");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Limited_Allocation
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/limited_allocation.in.xml");

      Allocator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/limited_allocation.ref.xml",
                   Filename2 => "obj/system.xml"),
              Message => "Limited allocation");
   end Limited_Allocation;

   -------------------------------------------------------------------------

   procedure Overlap_Between_Device_Memory
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/overlap_between_device_memory.xml");
      Allocator.Write (Policy     => Policy,
                       Output_Dir => "obj");
      pragma Unreferenced (Policy);
      Fail ("Overlap undetected");
   exception
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
   end Overlap_Between_Device_Memory;

   -------------------------------------------------------------------------

   procedure Overlap_Between_Devices
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/overlap_between_devices.xml");
      Allocator.Write (Policy     => Policy,
                       Output_Dir => "obj");
      pragma Unreferenced (Policy);
      Fail ("Overlap undetected");
   exception
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
   end Overlap_Between_Devices;

   -------------------------------------------------------------------------

   procedure Overlapping_Devices
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/overlapping_device.xml");
      Allocator.Write (Policy     => Policy,
                       Output_Dir => "obj");
      pragma Unreferenced (Policy);
      Fail ("Overlap undetected");
   exception
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
   end Overlapping_Devices;

   -------------------------------------------------------------------------

   procedure Overlapping_Physical_Memory
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/overlapping.xml");
      Allocator.Write (Policy     => Policy,
                       Output_Dir => "obj");
      pragma Unreferenced (Policy);
      Fail ("Overlap undetected");
   exception
      --  Should raise an exception.
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
   end Overlapping_Physical_Memory;

end Allocation_Tests;
