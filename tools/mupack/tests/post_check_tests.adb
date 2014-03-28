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

with Pack.Image;
with Pack.Post_Checks;
with Pack.Content_Providers;

package body Post_Check_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Post-check tests");
      T.Add_Test_Routine
        (Routine => Multiboot_Header'Access,
         Name    => "Multiboot header");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Multiboot_Header
   is
      Data : Content_Providers.Param_Type (16#102000#);
   begin
      begin
         Post_Checks.Multiboot_Header (Data => Data);
         Fail (Message => "Exception expected");

      exception
         when Post_Checks.Check_Error => null;
      end;

      Image.Add_Buffer (Image   => Data.Image,
                        Buffer  => (16#02#, 16#b0#, 16#ad#, 16#1b#),
                        Address => 16#101ffc#);
      Post_Checks.Multiboot_Header (Data => Data);
   end Multiboot_Header;

end Post_Check_Tests;
