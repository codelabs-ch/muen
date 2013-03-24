--
-- Copyright (c) 2008-2010 Tero Koskinen <tero.koskinen@iki.fi>
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
with Ahven.Results;
with Ahven.AStrings;

use Ahven;
use Ahven.AStrings;

package body Results_Tests is
   procedure Assert_Eq_Int is
     new Ahven.Assert_Equal (Data_Type => Integer,
                             Image     => Integer'Image);

   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Ahven.Results");
      Add_Test_Routine (T, Test_Count_Children'Access, "Test Count Children");
      Add_Test_Routine (T, Test_Direct_Count'Access, "Test Direct Count");
      Add_Test_Routine (T, Test_Result_Iterator'Access,
                        "Test Result Iterator");
      Add_Test_Routine (T, Test_Add_Pass'Access, "Test Add Pass");
      Add_Test_Routine (T, Test_Add_Failure'Access, "Test Add Failure");
      Add_Test_Routine (T, Test_Add_Error'Access, "Test Add Error");
   end Initialize;

   procedure Test_Count_Children is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : constant Result_Info := Empty_Result_Info;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert_Eq_Int (Actual   => Test_Count (Coll),
                     Expected => 2,
                     Message  => "test count");
   end Test_Count_Children;

   procedure Test_Direct_Count is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : constant Result_Info := Empty_Result_Info;
      Expected_Test_Count : constant := 3;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Failure (Coll, Info);
      Add_Pass (Coll, Info);

      -- This should not be counted in direct test count
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert_Eq_Int (Actual => Direct_Test_Count (Coll),
                     Expected => Expected_Test_Count,
                     Message => "test count");
      Assert_Eq_Int (Actual => Direct_Test_Count (Coll_Dyn.all),
                     Expected => 1,
                     Message => "test count (dyn)");
   end Test_Direct_Count;

   procedure Test_Result_Iterator is
      use Ahven.Results;

      Msg  : constant Bounded_String := To_Bounded_String ("hello");

      function Count_Tests (Position : Result_Info_Cursor) return Integer is
         Count : Natural            := 0;
         Pos   : Result_Info_Cursor := Position;
      begin
         loop
            exit when not Is_Valid (Pos);
            Assert (Get_Message (Data (Pos)) = To_String (Msg),
                    "Invalid message in the item");
            Pos := Next (Pos);
            Count := Count + 1;
         end loop;

         return Count;
      end Count_Tests;

      Coll : Result_Collection;
      Info : Result_Info := Empty_Result_Info;

      Error_Amount   : constant := 1;
      Failure_Amount : constant := 2;
      Pass_Amount    : constant := 3;
   begin
      Set_Message (Info, Msg);
      for I in 1 .. Error_Amount loop
         Add_Error (Coll, Info);
      end loop;
      for I in 1 .. Failure_Amount loop
         Add_Failure (Coll, Info);
      end loop;
      for I in 1 .. Pass_Amount loop
         Add_Pass (Coll, Info);
      end loop;

      Assert_Eq_Int (Actual   => Count_Tests (First_Pass (Coll)),
                     Expected => Pass_Amount,
                     Message  => "pass amount");

      Assert_Eq_Int (Actual   => Count_Tests (First_Failure (Coll)),
                     Expected => Failure_Amount,
                     Message  => "failure amount");

      Assert_Eq_Int (Actual   => Count_Tests (First_Error (Coll)),
                     Expected => Error_Amount,
                     Message  => "error amount");
   end Test_Result_Iterator;

   procedure Test_Add_Pass is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Pass (Coll, Info);
      Assert (Pass_Count (Coll) = 1, "Pass was not added!");
   end Test_Add_Pass;

   procedure Test_Add_Failure is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Failure (Coll, Info);
      Assert (Failure_Count (Coll) = 1, "Failure was not added!");
   end Test_Add_Failure;

   procedure Test_Add_Error is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Error (Coll, Info);
      Assert (Error_Count (Coll) = 1, "Error was not added!");
   end Test_Add_Error;

end Results_Tests;
