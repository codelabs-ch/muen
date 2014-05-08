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

with Musinfo.Utils;

package body Utils_Tests
is

   use Ahven;
   use Musinfo;

   -------------------------------------------------------------------------

   procedure Create_Channel
   is
      use type Interfaces.Unsigned_64;

      Ref_Name   : constant Name_Type := Utils.Create_Name (Str => "foo");
      Ref_Addr   : constant Interfaces.Unsigned_64 := 16#8000_cafe_beef_0000#;
      Ref_Size   : constant Interfaces.Unsigned_64 := 16#2000#;
      Ref_Event  : constant Event_Number_Range     := 234;
      Ref_Vector : constant Vector_Range           := 123;
      Channel    : Channel_Type;
   begin
      Channel := Utils.Create_Channel
        (Name       => Ref_Name,
         Address    => Ref_Addr,
         Size       => Ref_Size,
         Writable   => False,
         Has_Event  => True,
         Has_Vector => False,
         Event      => Ref_Event,
         Vector     => Ref_Vector);

      Assert (Condition => Channel.Name = Ref_Name,
              Message   => "Name mismatch");
      Assert (Condition => Channel.Address = Ref_Addr,
              Message   => "Address mismatch");
      Assert (Condition => Channel.Size = Ref_Size,
              Message   => "Size mismatch");
      Assert (Condition => not Channel.Writable,
              Message   => "Writable");
      Assert (Condition => Channel.Has_Event,
              Message   => "Has no event");
      Assert (Condition => not Channel.Has_Vector,
              Message   => "Has vector");
      Assert (Condition => Channel.Event = Ref_Event,
              Message   => "Event mismatch");
      Assert (Condition => Channel.Vector = Ref_Vector,
              Message   => "Vector mismatch");
   end Create_Channel;

   -------------------------------------------------------------------------

   procedure Create_Name
   is
      Ref_Str  : constant String := "foobar";
      Ref_Name : Name_Type       := Null_Name;
   begin
      Ref_Name.Length := Ref_Str'Length;
      Ref_Name.Data (1 .. Ref_Str'Length) := Name_Data_Type (Ref_Str);

      Assert (Condition => Utils.Create_Name (Str => Ref_Str) = Ref_Name,
              Message   => "Name mismatch");
   end Create_Name;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Utils tests");
      T.Add_Test_Routine
        (Routine => Create_Name'Access,
         Name    => "Create name from string");
      T.Add_Test_Routine
        (Routine => Create_Channel'Access,
         Name    => "Create channel");
   end Initialize;

end Utils_Tests;
