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

with Ahven;


--## rule off DIRECTLY_ACCESSED_GLOBALS
package body Dummy_Tests is
   Instance_Count : Integer := 0;

   procedure Initialize (T : in out Test) is
      procedure Register (T : in out Ahven.Framework.Test_Case'Class;
                          Routine : Ahven.Framework.Simple_Test_Routine_Access;
                          Name    : String)
        renames Ahven.Framework.Add_Test_Routine;
   begin
      Register (T, This_Test_Fails'Access, "Failure");
      Register (T, This_Test_Passes'Access, "Pass");
      Register (T, This_Test_Raises_Error'Access, "Error");
      Register (T, This_Test_Is_Skipped'Access, "Skipped");
      Ahven.Framework.Add_Test_Routine
        (T, This_Test_Uses_Object'Access, "Object usage");
      T.State := INITIALIZED;

      Instance_Count := Instance_Count + 1;
   end Initialize;

   procedure Adjust (T : in out Test) is
   begin
      Instance_Count := Instance_Count + 1;
   end Adjust;

   procedure Finalize (T : in out Test) is
   begin
      Instance_Count := Instance_Count - 1;
   end Finalize;

   procedure Set_Up (T : in out Test) is
   begin
      T.State := UP;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      T.State := DOWN;
   end Tear_Down;

   procedure This_Test_Fails is
   begin
      Ahven.Fail ("Failure");
   end This_Test_Fails;

   procedure This_Test_Passes is
   begin
      Ahven.Assert (True, "True was not true!");
   end This_Test_Passes;

   procedure This_Test_Raises_Error is
   begin
      raise Constraint_Error;
   end This_Test_Raises_Error;

   procedure This_Test_Is_Skipped is
   begin
      Ahven.Skip ("skipped");
   end This_Test_Is_Skipped;

   procedure This_Test_Uses_Object
     (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Test (T).State := USED;
   end This_Test_Uses_Object;

   procedure This_Test_Takes_12_Seconds is
   begin
      delay 12.0;
   end This_Test_Takes_12_Seconds;

   procedure This_Test_Has_Infinite_Loop is
      --## rule off Removable
      A : Integer := 0;
   begin
      loop
         A := 1;
      end loop;
      --## rule on Removable
   end This_Test_Has_Infinite_Loop;

   function Get_Instance_Count return Integer is
   begin
      return Instance_Count;
   end Get_Instance_Count;

   procedure Reset_Instance_Count is
   begin
      Instance_Count := 0;
   end Reset_Instance_Count;
end Dummy_Tests;
--## rule on DIRECTLY_ACCESSED_GLOBALS
