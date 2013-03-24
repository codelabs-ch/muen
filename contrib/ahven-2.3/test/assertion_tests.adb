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

with Ahven;

use Ahven;

package body Assertion_Tests is
   procedure Assert_Int_Equal is
     new Assert_Equal (Data_Type => Integer, Image => Integer'Image);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven");

      Framework.Add_Test_Routine
        (T, Test_Assert_Equal'Access, "Assert_Equal");

      Framework.Add_Test_Routine (T, Test_Assert'Access, "Assert");
      Framework.Add_Test_Routine (T, Test_Fail'Access, "Fail");
   end Initialize;

   procedure Test_Assert_Equal is
      Exception_Got : Boolean;
   begin
      begin
         Exception_Got := False;
         Assert_Int_Equal
           (Expected => 1, Actual => 1, Message => "Assert_Equal");
      exception
         when Assertion_Error =>
            Exception_Got := True;
      end;
      Assert (not Exception_Got, "exception for valid condition!");

      begin
         Exception_Got := False;
         Assert_Int_Equal
           (Expected => 1, Actual => 2, Message => "Assert_Equal");
      exception
         when Assertion_Error =>
            Exception_Got := True;
      end;
      Assert (Exception_Got, "no exception for invalid condition!");
   end Test_Assert_Equal;

   procedure Test_Fail is
      Exception_Got : Boolean;
   begin
      begin
         Exception_Got := False;
         Fail ("fail");
      exception
         when Assertion_Error =>
            Exception_Got := True;
      end;
      Assert (Exception_Got, "Fail did not raise exception!");
   end Test_Fail;

   procedure Test_Assert is
      Exception_Got : Boolean;
   begin
      Assert (True, "Assert (True)");

      begin
         Exception_Got := False;
         Assert (False, "assertion");
      exception
         when Assertion_Error =>
            Exception_Got := True;
      end;
      if not Exception_Got then
         -- Raising Assertion_Error directly, since Assert apparently
         -- does not work.
         raise Assertion_Error;
      end if;
   end Test_Assert;

end Assertion_Tests;
