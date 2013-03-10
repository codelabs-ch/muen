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

with Ada.Command_Line;

with Ahven.Listeners;
with Ahven.Listeners.Basic;

package body Ahven.Runner is
   use Ahven.Results;

   procedure Run_Suite (Suite    : in out Framework.Test'Class;
                        Reporter :        Report_Proc) is
      use Ahven.Listeners.Basic;

      Listener : Listeners.Basic.Basic_Listener;
      Params   : Parameters.Parameter_Info;
   begin
      Parameters.Parse_Parameters (Parameters.NORMAL_PARAMETERS, Params);
      Set_Output_Capture (Listener, Parameters.Capture (Params));

      if Parameters.Single_Test (Params) then
         Framework.Execute
           (T => Suite, Test_Name => Parameters.Test_Name (Params),
            Listener => Listener, Timeout => Parameters.Timeout (Params));
      else
         Framework.Execute (Suite, Listener, Parameters.Timeout (Params));
      end if;

      Reporter (Listener.Main_Result, Params);
      if (Error_Count (Listener.Main_Result) > 0) or
         (Failure_Count (Listener.Main_Result) > 0) then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   exception
      when Parameters.Invalid_Parameter =>
         Parameters.Usage;
   end Run_Suite;
end Ahven.Runner;
