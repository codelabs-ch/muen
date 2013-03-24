--
-- Copyright (c) 2008-2011 Tero Koskinen <tero.koskinen@iki.fi>
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
with Ada.Text_IO;

use Ada.Command_Line;
use Ada.Text_IO;

package body Ahven.Parameters is
   -- Possible options:
   --  -c : capture output
   --  -d : result directory
   --  -q : quiet mode
   --  -t : timeout
   --  -v : verbose mode (default)
   --  -x : XML output
   --
   procedure Parse_Options (Info     : in out Parameter_Info;
                            Mode     :        Parameter_Mode;
                            Option   :        String;
                            Dir_Next :    out Boolean;
                            Timeout_Next : out Boolean) is
      procedure Check_Invalid (C : Character) is
      begin
         case Mode is
            when NORMAL_PARAMETERS =>
               if C = 'n' then
                  raise Invalid_Parameter;
               end if;
            when TAP_PARAMETERS =>
               if (C = 'd') or (C = 'x') then
                  raise Invalid_Parameter;
               end if;
         end case;
      end Check_Invalid;
   begin
      Dir_Next := False;
      Timeout_Next := False;
      for A in Option'Range loop
         Check_Invalid (Option (A));
         case Option (A) is
            when 'c' =>
               Info.Capture_Output := True;
            when 'd' =>
               Dir_Next := True;
            when 't' =>
               Timeout_Next := True;
            when 'v' =>
               Info.Verbose_Output := True;
            when 'q' =>
               Info.Verbose_Output := False;
            when 'x' =>
               Info.Xml_Output := True;
            when others =>
               raise Invalid_Parameter;
         end case;
      end loop;
   end Parse_Options;

   -- Recognize command line parameters.
   -- Option "--" can be used to separate options and test names.
   --
   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info) is
      procedure Handle_Parameter (P     : in out Parameter_Info;
                                  Arg   :        String;
                                  Index :        Positive);
      -- Parse one parameter and update P if necessary.

      Files_Only  : Boolean := False;
      Dir_Next    : Boolean := False;
      Timeout_Next : Boolean := False;

      procedure Handle_Parameter (P     : in out Parameter_Info;
                                  Arg   :        String;
                                  Index :        Positive)
      is
      begin
         if Dir_Next then
            P.Result_Dir := Index;
            Dir_Next := False;
         elsif Timeout_Next then
            P.Timeout := Framework.Test_Duration'Value (Arg);
            Timeout_Next := False;
         elsif Arg = "--" then
            Files_Only := True;
         elsif Arg'Size > 1 then
            if (not Files_Only) and (Arg (Arg'First) = '-') then
               Parse_Options
                 (Info => P,
                  Mode => Mode,
                  Option => Arg (Arg'First + 1 .. Arg'Last),
                  Dir_Next => Dir_Next,
                  Timeout_Next => Timeout_Next);
            else
               P.Test_Name := Index;
            end if;
         end if;
      end Handle_Parameter;
   begin
      -- Default values
      Info := (Verbose_Output => True,
               Xml_Output     => False,
               Capture_Output => False,
               Test_Name      => 0,
               Result_Dir     => 0,
               Timeout        => 0.0);
      for A in Positive range 1 .. Argument_Count loop
         Handle_Parameter (Info, Argument (A), A);
      end loop;
      if Dir_Next then
         raise Invalid_Parameter;
      end if;
   end Parse_Parameters;

   procedure Usage (Mode : Parameter_Mode := NORMAL_PARAMETERS) is
   begin
      case Mode is
         when NORMAL_PARAMETERS =>
            Put_Line
              ("Possible parameters: [-cqvx] [-d directory] [--] [testname]");
            Put_Line ("   -d    : directory for test results");
            Put_Line ("   -x    : output in XML format");
         when TAP_PARAMETERS =>
            Put_Line ("Possible parameters: [-cqv] [--] [testname]");
      end case;
      Put_Line ("   -c    : capture and report test outputs");
      Put_Line ("   -q    : quiet results");
      Put_Line ("   -t    : test timeout, infinite default");
      Put_Line ("   -v    : verbose results (default)");
      Put_Line ("   --    : end of parameters (optional)");
   end Usage;

   function Capture (Info : Parameter_Info) return Boolean is
   begin
      return Info.Capture_Output;
   end Capture;

   function Verbose (Info : Parameter_Info) return Boolean is
   begin
      return Info.Verbose_Output;
   end Verbose;

   function XML_Results (Info : Parameter_Info) return Boolean is
   begin
      return Info.Xml_Output;
   end XML_Results;

   function Single_Test (Info : Parameter_Info) return Boolean is
   begin
      return (Info.Test_Name /= 0);
   end Single_Test;

   function Test_Name (Info : Parameter_Info) return String is
   begin
      if Info.Test_Name = 0 then
         return "";
      else
         return Argument (Info.Test_Name);
      end if;
   end Test_Name;

   function Result_Dir (Info : Parameter_Info) return String is
   begin
      if Info.Result_Dir = 0 then
         return "";
      else
         return Argument (Info.Result_Dir);
      end if;
   end Result_Dir;

   function Timeout (Info : Parameter_Info) return Framework.Test_Duration is
   begin
      return Info.Timeout;
   end Timeout;
end Ahven.Parameters;
