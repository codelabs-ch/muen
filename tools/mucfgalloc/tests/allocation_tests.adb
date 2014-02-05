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

   procedure Automatic_Allocation
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/automatic_allocation.in.xml");

      Allocator.Write (Output_File => "obj/automatic_allocation.out.xml",
                       Policy      => Policy);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/automatic_allocation.ref.xml",
                   Filename2 => "obj/automatic_allocation.out.xml"),
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
   end Initialize;

   -------------------------------------------------------------------------

   procedure Limited_Allocation
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/limited_allocation.in.xml");

      Allocator.Write (Output_File => "obj/limited_allocation.out.xml",
                       Policy      => Policy);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/limited_allocation.ref.xml",
                   Filename2 => "obj/limited_allocation.out.xml"),
              Message => "Limited allocation");
   end Limited_Allocation;

   -------------------------------------------------------------------------

   procedure Overlapping_Physical_Memory
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/overlapping.xml");
      Allocator.Write (Policy      => Policy,
                       Output_File => "obj/overlapping.xml");
      pragma Unreferenced (Policy);
      Fail ("Overlap undetected");
   exception
      --  Should raise an exception.
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
   end Overlapping_Physical_Memory;

end Allocation_Tests;
