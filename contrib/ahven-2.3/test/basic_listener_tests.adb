--
-- Copyright (c) 2008 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ahven;
with Ahven.Listeners.Basic;
with Ahven.Results;
with Ahven.AStrings;
with Ahven.Long_AStrings;

use Ahven;
use Ahven.Results;

package body Basic_Listener_Tests is

   procedure Assert_Equal_Nat is new Ahven.Assert_Equal
     (Data_Type => Natural, Image => Natural'Image);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Listeners.Basic");
      Framework.Add_Test_Routine
        (T, Test_Single_Pass'Access, "Test Single Pass");
      Framework.Add_Test_Routine
        (T, Test_Error_Inside_Suite'Access, "Test Error Inside Suite");
   end Initialize;

   procedure Test_Single_Pass is
      use Ahven.Listeners;
      use Ahven.AStrings;

      Listener : Basic.Basic_Listener;
   begin
      Listeners.Basic.Start_Test
        (Listener, (Phase     => TEST_BEGIN,
                    Test_Name => To_Bounded_String ("testname"),
                    Test_Kind => ROUTINE));
      Listeners.Basic.Add_Pass
        (Listener,
          (Phase        => TEST_RUN,
           Test_Name    => To_Bounded_String ("testname"),
           Test_Kind    => ROUTINE,
           Routine_Name => To_Bounded_String ("routine"),
           Message      => To_Bounded_String ("message"),
           Long_Message => Long_AStrings.To_Bounded_String ("long_message")));
      Listeners.Basic.End_Test
        (Listener, (Phase          => TEST_END,
                    Test_Name      => To_Bounded_String ("testname"),
                    Test_Kind      => ROUTINE));

      Assert_Equal_Nat (Test_Count (Listener.Main_Result), 1, "Test Count");
   end Test_Single_Pass;

   procedure Test_Error_Inside_Suite is
      use Ahven.Listeners;
      use Ahven.AStrings;

      Listener : Basic.Basic_Listener;
   begin
      Listeners.Basic.Start_Test
        (Listener, (Phase     => TEST_BEGIN,
                    Test_Name => To_Bounded_String ("suite"),
                    Test_Kind => CONTAINER));

      Listeners.Basic.Start_Test
        (Listener, (Phase     => TEST_BEGIN,
                    Test_Name => To_Bounded_String ("testname"),
                    Test_Kind => ROUTINE));

      Listeners.Basic.Add_Error
        (Listener,
          (Phase     => TEST_RUN,
           Test_Name => To_Bounded_String ("testname"),
           Test_Kind => ROUTINE,
           Routine_Name => To_Bounded_String ("routine"),
           Message      => To_Bounded_String ("message"),
           Long_Message => Long_AStrings.To_Bounded_String ("long_message")));
      Listeners.Basic.End_Test
        (Listener, (Phase     => TEST_END,
                    Test_Name => To_Bounded_String ("testname"),
                    Test_Kind => ROUTINE));

      Listeners.Basic.End_Test
        (Listener, (Phase     => TEST_END,
                    Test_Name => To_Bounded_String ("suite"),
                    Test_Kind => CONTAINER));

      Assert_Equal_Nat (Test_Count (Listener.Main_Result), 1, "Test Count");

      Assert_Equal_Nat (Direct_Test_Count (Listener.Main_Result), 0,
                        "Direct Test Count");

      Assert_Equal_Nat (Error_Count (Listener.Main_Result), 1, "Error Count");
   end Test_Error_Inside_Suite;

end Basic_Listener_Tests;
