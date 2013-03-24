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

with Ada.Calendar;
with Ahven.Temporary_Output;
with Ahven.Results;

use Ahven.Results;

pragma Elaborate_All (Ahven.Results);
pragma Elaborate_All (Ahven.Temporary_Output);

package Ahven.Listeners.Basic is
   type Result_Type is
     (NO_RESULT, PASS_RESULT, FAILURE_RESULT, ERROR_RESULT, SKIPPED_RESULT);

   type Basic_Listener is new Result_Listener with record
      Main_Result       : aliased Result_Collection;
      Current_Result    : Result_Collection_Access;
      Last_Test_Result  : Result_Type := NO_RESULT;
      Last_Info         : Result_Info := Empty_Result_Info;
      Capture_Output    : Boolean     := False;
      Output_File       : Temporary_Output.Temporary_File;
      Start_Time        : Ada.Calendar.Time;
   end record;

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Info     :        Context);
   -- New implementation for Listeners.Add_Pass

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Info     :        Context);
   -- New implementation for Listeners.Add_Failure

   procedure Add_Skipped (Listener : in out Basic_Listener;
                          Info     :        Context);
   -- New implementation for Listeners.Add_Skipped

   procedure Add_Error (Listener : in out Basic_Listener;
                        Info     :        Context);
   -- New implementation for Listeners.Add_Error

   procedure Start_Test (Listener : in out Basic_Listener;
                         Info     :        Context);
   -- New implementation for Listeners.Start_Test

   procedure End_Test (Listener : in out Basic_Listener;
                       Info     :        Context);
   -- New implementation for Listeners.End_Test

   procedure Set_Output_Capture (Listener : in out Basic_Listener;
                                 Capture  :        Boolean);
   -- Enable or disable Ada.Text_IO output capturing

   function Get_Output_Capture (Listener : Basic_Listener)
     return Boolean;
   -- Capture the Ada.Text_IO output?

private
   procedure Set_Last_Test_Info (Listener : in out Basic_Listener;
                                 Info     :        Context;
                                 Result   :        Result_Type);

   procedure Remove_File (Name : String);
   procedure Remove_Files (Collection : in out Result_Collection);

   procedure Finalize (Listener : in out Basic_Listener);

end Ahven.Listeners.Basic;
