--
-- Copyright (c) 2008-2009 Tero Koskinen <tero.koskinen@iki.fi>
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
with Ahven.Framework;
with Ahven.Listeners;
with Ahven.Temporary_Output;

package Ahven.Tap_Runner is
   use Ahven.Listeners;

   procedure Run (Suite : in out Framework.Test'Class);
   -- Run the suite and print the results.
private
   type Tap_Result_Type is (OK_RESULT, NOT_OK_RESULT);

   type Tap_Listener is new Ahven.Listeners.Result_Listener with record
      Result : Tap_Result_Type := NOT_OK_RESULT;
      Current_Test : Framework.Test_Count_Type := 0;
      Verbose : Boolean := True;
      Output_File : Temporary_Output.Temporary_File;
      Capture_Output : Boolean := False;
   end record;

   procedure Add_Pass (Listener : in out Tap_Listener;
                       Info     :        Context);

   procedure Add_Failure (Listener : in out Tap_Listener;
                          Info     :        Context);

   procedure Add_Error (Listener : in out Tap_Listener;
                        Info     :        Context);

   procedure Add_Skipped (Listener : in out Tap_Listener;
                          Info     :        Context);

   procedure Start_Test (Listener : in out Tap_Listener;
                         Info     :        Context);

   procedure End_Test (Listener : in out Tap_Listener;
                       Info     :        Context);
end Ahven.Tap_Runner;
