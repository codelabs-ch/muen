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

with Expanders.Channels;

with Test_Utils.Expander;

package body Channels_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Add_Memory_Regions
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/channels_memory.xml",
         Ref_Filename => "data/channels_memory.ref.xml",
         Expander     => Expanders.Channels.Add_Physical_Memory'Access);
   end Add_Memory_Regions;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Channels expander tests");
      T.Add_Test_Routine
        (Routine => Add_Memory_Regions'Access,
         Name    => "Add memory regions");
   end Initialize;

end Channels_Tests;
