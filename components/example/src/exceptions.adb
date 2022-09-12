--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;

with System.Machine_Code;

with Backtraces;
with Log;

package body Exceptions
is

   -------------------------------------------------------------------------

   procedure Print_Backtrace
     (RIP : Interfaces.Unsigned_64;
      RBP : Interfaces.Unsigned_64)
   is

      --  The array range is arbitrarily chosen but 10 seems high enough for the
      --  call depth of this example test case.

      Call_Traces : Backtraces.Call_Trace_Type (1 .. 10);
      Last_Entry : Natural;
   begin
      Backtraces.Analyse_Stack
        (RIP        => RIP,
         RBP        => RBP,
         Traces     => Call_Traces,
         Last_Index => Last_Entry);

      Log.Put_Line (Item => "Call Trace:");
      for Idx in Call_Traces'First .. Last_Entry loop
         Log.Put_Line
           (Item => " [<"
            & SK.Strings.Img (Item => Call_Traces (Idx)) & ">]");
      end loop;
   end Print_Backtrace;

   -------------------------------------------------------------------------

   procedure Trigger_Breakpoint
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "int3",
         Clobber  => "memory",
         Volatile => True);
   end Trigger_Breakpoint;

end Exceptions;
