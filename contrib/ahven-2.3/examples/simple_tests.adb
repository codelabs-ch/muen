--
-- Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Text_IO;
use Ada.Text_IO;

package body Simple_Tests is
   type Test_Access is access all Test;

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Simple Tests");

      Ahven.Framework.Add_Test_Routine
        (T, Test_Assertion'Access, "Test Assertion");
      Ahven.Framework.Add_Test_Routine
        (T, Test_With_Object'Access, "Test With Object");
      Ahven.Framework.Add_Test_Routine
        (T, Test_Error'Access, "Test Error (exception)");

   end Initialize;
   
   procedure Set_Up (T : in out Test) is
   begin
      Put_Line ("Simple_Tests.Set_Up");
      T.Value := 1;
   end Set_Up;
   
   procedure Tear_Down (T : in out Test) is
   begin
      Put_Line ("Simple_Tests.Tear_Down");
      T.Value := -1;
   end Tear_Down;
   
   procedure Test_Assertion is
   begin
      Put_Line ("Test_Assertion");
      Ahven.Assert (False, "assert(false)");
   end Test_Assertion;
   
   procedure Test_Error is
   begin
      raise Constraint_Error;
   end Test_Error;

   procedure Hello (T : Test) is
   begin
      Ahven.Assert (T.Value = 1, "T.Value = 1");
   end Hello;
   
   procedure Test_With_Object (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Put_Line ("Test_With_Object");
      Hello (Test (T));
   end Test_With_Object;
end Simple_Tests;

