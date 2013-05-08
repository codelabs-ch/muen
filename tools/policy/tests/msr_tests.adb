with Ada.Streams;

with Skp.MSRs;

package body MSR_Tests
is

   use Ahven;
   use Skp;

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
      use type Ada.Streams.Stream_Element_Array;

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
                       Mode       => MSR_Read_Write);
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#c0000000#,
                       End_Addr   => 16#c0001fff#,
                       Mode       => MSR_Read_Write);
      Assert (Condition => MSRs.To_Stream (Bitmap => B) = Full_Ref,
              Message   => "Full MSR bitmap mismatch");

      B := MSRs.Null_MSR_Bitmap;
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#c0000000#,
                       End_Addr   => 16#c0000007#,
                       Mode       => MSR_Read);
      MSRs.Allow_MSRs (Bitmap     => B,
                       Start_Addr => 16#80#,
                       End_Addr   => 16#87#,
                       Mode       => MSR_Write);
      Assert (Condition => MSRs.To_Stream (Bitmap => B) = RW_Ref,
              Message   => "RW MSR bitmap mismatch");
   end MSR_Bitmap_Handling;

end MSR_Tests;
