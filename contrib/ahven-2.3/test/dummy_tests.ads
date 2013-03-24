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

with Ahven.Framework;

package Dummy_Tests is
   Dummy_Passes     : constant := 2;
   Dummy_Failures   : constant := 1;
   Dummy_Errors     : constant := 1;
   Dummy_Skips      : constant := 1;
   Dummy_Test_Count : constant := Dummy_Passes + Dummy_Failures + Dummy_Errors
     + Dummy_Skips;

   type Test_State is (INITIALIZED, UP, DOWN, USED);

   type Test is new Ahven.Framework.Test_Case with record
      State : Test_State;
   end record;

   procedure Initialize (T : in out Test);

   procedure Adjust (T : in out Test);

   procedure Finalize (T : in out Test);

   procedure Set_Up (T : in out Test);

   procedure Tear_Down (T : in out Test);

   procedure This_Test_Fails;

   procedure This_Test_Passes;

   procedure This_Test_Raises_Error;

   procedure This_Test_Is_Skipped;

   procedure This_Test_Uses_Object
     (T : in out Ahven.Framework.Test_Case'Class);

   procedure This_Test_Takes_12_Seconds;

   procedure This_Test_Has_Infinite_Loop;

   function Get_Instance_Count return Integer;

   procedure Reset_Instance_Count;

end Dummy_Tests;
