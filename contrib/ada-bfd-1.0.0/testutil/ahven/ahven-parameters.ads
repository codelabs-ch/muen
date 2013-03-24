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

with Ahven.Framework;

package Ahven.Parameters is

   Invalid_Parameter : exception;

   type Parameter_Info is private;

   type Parameter_Mode is (NORMAL_PARAMETERS, TAP_PARAMETERS);

   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info);
   -- Parse Ada.Command_Line parameters and put the results
   -- to the Info parameter. Raises Invalid_Parameter if
   -- some parameter is invalid.

   procedure Usage (Mode : Parameter_Mode := NORMAL_PARAMETERS);
   -- Print usage.

   function Capture (Info : Parameter_Info) return Boolean;
   -- Capture Ada.Text_IO output?

   function Verbose (Info : Parameter_Info) return Boolean;
   -- Use verbose mode?

   function XML_Results (Info : Parameter_Info) return Boolean;
   -- Output XML?

   function Single_Test (Info : Parameter_Info) return Boolean;
   -- Run a single test (case/suite/routine) only?

   function Test_Name (Info : Parameter_Info) return String;
   -- Return the name of the test passed as a parameter.

   function Result_Dir (Info : Parameter_Info) return String;
   -- Return the directory for XML results.

   function Timeout (Info : Parameter_Info) return Framework.Test_Duration;
   -- Return the timeout value for a test.

private
   type Parameter_Info is record
      Verbose_Output : Boolean := True;
      Xml_Output     : Boolean := False;
      Capture_Output : Boolean := False;

      Test_Name      : Natural := 0;
      -- Position of test name in the argument array

      Result_Dir     : Natural := 0;
      -- Position of results dir in the argument array

      Timeout        : Framework.Test_Duration := 0.0;
   end record;
end Ahven.Parameters;
