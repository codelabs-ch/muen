--
-- Copyright (c) 2007-2009 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Exceptions;

pragma Elaborate_All (Ada.Exceptions);

package body Ahven is

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Ada.Exceptions.Raise_Exception (Assertion_Error'Identity, Message);
      end if;
   end Assert;

   procedure Assert_Equal
      (Actual : Data_Type; Expected : Data_Type; Message : String) is
   begin
      Assert (Actual = Expected, Message &
              " (Expected: " & Image (Expected) &
              "; Got: " & Image (Actual) & ")");
   end Assert_Equal;

   procedure Fail (Message : String) is
   begin
      Ada.Exceptions.Raise_Exception (Assertion_Error'Identity, Message);
   end Fail;

   procedure Skip (Message : String) is
   begin
      Ada.Exceptions.Raise_Exception (Test_Skipped_Error'Identity, Message);
   end Skip;

end Ahven;
