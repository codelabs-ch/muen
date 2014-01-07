--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Msrbm.MSRs;

package body MSR_Tests
is

   use Ahven;
   use Msrbm;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "MSR tests");
      T.Add_Test_Routine
        (Routine => MSR_Bitmap_Handling'Access,
         Name    => "MSR bitmap handling");
   end Initialize;

   -------------------------------------------------------------------------

   procedure MSR_Bitmap_Handling
   is
      use type MSRs.MSR_Bitmap_Stream;

      B        : MSRs.MSR_Bitmap_Type := MSRs.Null_MSR_Bitmap;
      Null_Ref : constant MSRs.MSR_Bitmap_Stream := (others => 16#ff#);
      Full_Ref : constant MSRs.MSR_Bitmap_Stream := (others => 16#00#);
      RW_Ref   : constant MSRs.MSR_Bitmap_Stream := (1024   => 16#00#,
                                                     2064   => 16#00#,
                                                     others => 16#ff#);
   begin
      Assert (Condition => MSRs.To_Stream (Bitmap => B) = Null_Ref,
              Message   => "Null MSR bitmap mismatch");

      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 0,
                       End_Addr   => 16#1fff#,
                       Mode       => MSRs.RW);
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#c0000000#,
                       End_Addr   => 16#c0001fff#,
                       Mode       => MSRs.RW);
      Assert (Condition => MSRs.To_Stream (Bitmap => B) = Full_Ref,
              Message   => "Full MSR bitmap mismatch");

      B := MSRs.Null_MSR_Bitmap;
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#c0000000#,
                       End_Addr   => 16#c0000007#,
                       Mode       => MSRs.R);
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#80#,
                       End_Addr   => 16#87#,
                       Mode       => MSRs.W);
      Assert (Condition => MSRs.To_Stream (Bitmap => B) = RW_Ref,
              Message   => "RW MSR bitmap mismatch");
   end MSR_Bitmap_Handling;

end MSR_Tests;
