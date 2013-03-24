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

with Simple_Listener;
with Dummy_Tests;

package body Static_Test_Case_Tests is
   use Ahven;
   use Ahven.Framework;

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   My_Test : Dummy_Tests.Test;
   Child   : Test_Suite := Create_Suite ("Child suite");

   procedure Assert_Eq_Nat is
     new Ahven.Assert_Equal (Data_Type => Natural, Image => Natural'Image);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Framework.Static");

      Framework.Add_Test_Routine (T, Test_Test_Suite_Run'Access,
                                  "Test_Suite: Run (Static)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Inside_Suite'Access,
                                  "Test_Suite: Suite inside another (Static)");
   end Initialize;

   procedure Test_Test_Suite_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Suite    : Test_Suite := Create_Suite ("My suite");
   begin
      Add_Static_Test (My_Suite, My_Test);

      Run (My_Suite, My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "passes");

      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "errors");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "failures");
      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 1),
              "Start_Test calls");
   end Test_Test_Suite_Run;

   procedure Test_Test_Suite_Inside_Suite is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      Parent      : Test_Suite := Create_Suite ("Parent suite");
   begin
      Framework.Add_Static_Test (Child, My_Test);

      Framework.Add_Static_Test (Parent, Child);

      Framework.Run (Parent, My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "passes");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "errors");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "failures");
      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 2),
              "Start_Test calls");
   end Test_Test_Suite_Inside_Suite;
end Static_Test_Case_Tests;
