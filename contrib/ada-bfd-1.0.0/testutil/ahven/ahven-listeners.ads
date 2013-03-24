--
-- Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Finalization;
with Ahven.AStrings;

package Ahven.Listeners is
   type Test_Phase is (TEST_BEGIN, TEST_RUN, TEST_END);
   -- What is test doing right now?

   type Test_Type is (CONTAINER, ROUTINE);

   type Context (Phase : Test_Phase) is record
      Test_Name : AStrings.Bounded_String;
      Test_Kind : Test_Type;
      case Phase is
         when TEST_BEGIN | TEST_END =>
            null;
         when TEST_RUN =>
            Routine_Name : AStrings.Bounded_String;
            Message      : AStrings.Bounded_String;
            Long_Message : AStrings.Bounded_String;
      end case;
   end record;

   type Result_Listener is
     abstract new Ada.Finalization.Limited_Controlled with null record;
   -- Result_Listener is a listener for test results.
   -- Whenever a test is run, the framework calls
   -- registered listeners and tells them the result of the test.

   type Result_Listener_Class_Access is access all Result_Listener'Class;

   procedure Add_Pass (Listener : in out Result_Listener;
                       Info     :        Context) is abstract;
   -- Called after test passes.

   procedure Add_Failure (Listener : in out Result_Listener;
                          Info     :        Context) is abstract;
   -- Called after test fails.

   procedure Add_Skipped (Listener : in out Result_Listener;
                          Info     :        Context);
   -- Called when user wants to skip the test.

   procedure Add_Error (Listener : in out Result_Listener;
                        Info     :        Context) is abstract;
   -- Called after there is an error in the test.

   procedure Start_Test (Listener : in out Result_Listener;
                         Info     :        Context) is abstract;
   -- Called before the test begins. This is called before Add_* procedures.

   procedure End_Test (Listener : in out Result_Listener;
                       Info     :        Context) is abstract;
   -- Called after the test ends. Add_* procedures are called before this.
end Ahven.Listeners;
