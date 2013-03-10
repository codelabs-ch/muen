------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2009, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with Util.Strings;
with Util.Strings.Transforms;

with AUnit.Time_Measure;

--  Very simple reporter to console
package body Util.Tests.Reporter is

   use AUnit.Test_Results;
   use type AUnit.Message_String;
   use type AUnit.Time_Measure.Time;
   use Ada.Text_IO;

   procedure Print_Summary (R : in out Result'Class);

   procedure Dump_Result_List (File : in Ada.Text_IO.File_Type;
                               L    : in Result_Lists.List);
   --  List failed assertions

--     procedure Put_Measure is new Gen_Put_Measure;
   --  Output elapsed time

   procedure Report_Test (File : in Ada.Text_IO.File_Type;
                          Test : in Test_Result);
   --  Report a single assertion failure or unexpected exception

   procedure Put (File : in Ada.Text_IO.File_Type;
                  I    : in Integer);

   procedure Put (File : in Ada.Text_IO.File_Type;
                  T    : in AUnit.Time_Measure.Time);

   procedure Put (File : in Ada.Text_IO.File_Type;
                  I    : in Integer) is
   begin
      Ada.Text_IO.Put (File, Util.Strings.Image (I));
   end Put;

   procedure Put (File : in Ada.Text_IO.File_Type;
                  T    : in AUnit.Time_Measure.Time) is
      use Ada.Calendar;

      D   : constant Duration := T.Stop - T.Start;
      S   : constant String   := Duration'Image (D);
      Pos : Natural := S'Last;
   begin
      while Pos > S'First and S (Pos) = '0' loop
         Pos := Pos - 1;
      end loop;
      if D >= 0.0 then
         Put (File, S (S'First + 1 .. Pos));
      else
         Put (File, S (S'First .. Pos));
      end if;
   end Put;

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (File : in Ada.Text_IO.File_Type;
                               L    : in Result_Lists.List) is

      use Result_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (File, Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   ------------
   -- Report --
   ------------

   procedure Report (Engine : XML_Reporter;
                     R      : in out Result'Class)
   is
      Output : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => Output,
                          Mode => Ada.Text_IO.Out_File,
                          Name => To_String (Engine.File));
      Engine.Report (Output, R);
      Ada.Text_IO.Close (Output);
   end Report;

   procedure Print_Summary (R : in out Result'Class) is
      S_Count : constant Integer := Integer (Success_Count (R));
      F_Count : constant Integer := Integer (Failure_Count (R));
      E_Count : constant Integer := Integer (Error_Count (R));
   begin
      New_Line;
      Put ("Total Tests Run:   ");
      Put (Util.Strings.Image (Integer (Test_Count (R))));
      New_Line;
      Put ("Successful Tests:  ");
      Put (Util.Strings.Image (S_Count));
      New_Line;
      Put ("Failed Assertions: ");
      Put (Util.Strings.Image (F_Count));
      New_Line;
      Put ("Unexpected Errors: ");
      Put (Util.Strings.Image (E_Count));
      New_Line;
   end Print_Summary;

   ------------
   -- Report --
   ------------
   procedure Report (Engine : XML_Reporter;
                     File   : in out Ada.Text_IO.File_Type;
                     R      : in out Result'Class)
   is
      pragma Unreferenced (Engine);
   begin
      Put_Line (File, "<?xml version='1.0' encoding='utf-8' ?>");
      Put      (File, "<TestRun");

      if Elapsed  (R) /= AUnit.Time_Measure.Null_Time then
         Put (File, " elapsed='");
         Put (File, Elapsed (R));
         Put_Line (File, "'>");
      else
         Put_Line (File, ">");
      end if;

      Print_Summary (R);

      Put_Line (File, "  <Statistics>");
      Put      (File, "    <Tests>");
      Put (File, Integer (Test_Count (R)));
      Put_Line (File, "</Tests>");
      Put      (File, "    <FailuresTotal>");
      Put (File, Integer (Failure_Count (R)) + Integer (Error_Count (R)));
      Put_Line (File, "</FailuresTotal>");
      Put      (File, "    <Failures>");
      Put (File, Integer (Failure_Count (R)));
      Put_Line (File, "</Failures>");
      Put      (File, "    <Errors>");
      Put (File, Integer (Error_Count (R)));
      Put_Line (File, "</Errors>");
      Put_Line (File, "  </Statistics>");

      declare
         S : Result_Lists.List;
      begin
         Put_Line (File, "  <SuccessfulTests>");
         Successes (R, S);
         Dump_Result_List (File, S);
         Put_Line (File, "  </SuccessfulTests>");
      end;

      Put_Line (File, "  <FailedTests>");
      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (File, F);
      end;

      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (File, E);
      end;
      Put_Line (File, "  </FailedTests>");

      Put_Line (File, "</TestRun>");
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Test (File : in Ada.Text_IO.File_Type;
                          Test : in Test_Result) is

      use Util.Strings.Transforms;
      use type Ada.Calendar.Time;

      Is_Assert : Boolean;
   begin
      Put (File, "    <Test");
      if Test.Elapsed /= AUnit.Time_Measure.Null_Time then
         Put (File, " elapsed='");
         Put (File, Test.Elapsed);
         Put_Line (File, "'>");
      else
         Put_Line (File, ">");
      end if;
      Put      (File, "      <Name>");
      Put      (File, Escape_Xml (Test.Test_Name.all));

      if Test.Routine_Name /= null then
         Put (File, " : ");
         Put (File, Escape_Xml (Test.Routine_Name.all));
      end if;

      Put_Line (File, "</Name>");

      if Test.Failure /= null or else Test.Error /= null then
         if Test.Failure /= null then
            Is_Assert := True;
         else
            Is_Assert := False;
         end if;

         Put      (File, "      <FailureType>");

         if Is_Assert then
            Put   (File, "Assertion");
         else
            Put   (File, "Error");
         end if;

         Put_Line (File, "</FailureType>");
         Put      (File, "      <Message>");
         if Is_Assert then
            Put   (File, Escape_Xml (Test.Failure.Message.all));
         else
            Put   (File, Test.Error.Exception_Name.all);
         end if;
         Put_Line (File, "</Message>");

         if Is_Assert then
            Put_Line (File, "      <Location>");
            Put      (File, "        <File>");
            Put      (File, Escape_Xml (Test.Failure.Source_Name.all));
            Put_Line (File, "</File>");
            Put      (File, "        <Line>");
            Put      (File, Test.Failure.Line);
            Put_Line (File, "</Line>");
            Put_Line (File, "      </Location>");

         else
            Put_Line (File, "      <Exception>");
            Put      (File, "      <Message>");
            Put      (File, Test.Error.Exception_Name.all);
            Put_Line (File, "</Message>");

            if Test.Error.Exception_Message /= null then
               Put      (File, "      <Information>");
               Put      (File, Escape_Xml (Test.Error.Exception_Message.all));
               Put_Line (File, "</Information>");
            end if;

            if Test.Error.Traceback /= null then
               Put      (File, "      <Traceback>");
               Put      (File, Escape_Xml (Test.Error.Traceback.all));
               Put_Line (File, "</Traceback>");
            end if;

            Put_Line (File, "      </Exception>");
         end if;
      end if;

      Put_Line (File, "    </Test>");
   end Report_Test;

end Util.Tests.Reporter;
