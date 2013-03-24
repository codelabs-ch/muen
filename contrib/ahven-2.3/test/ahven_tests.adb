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

with Framework_Tests;
with Derived_Tests;
with Results_Tests;
with Basic_Listener_Tests;
with Assertion_Tests;
with Static_Test_Case_Tests;
with SList_Tests;

package body Ahven_Tests is
   use Ahven;

   function Get_Test_Suite return Ahven.Framework.Test_Suite is
      S : Framework.Test_Suite := Framework.Create_Suite ("All");

      Assertion_Test : Assertion_Tests.Test;
      Derived_Test   : Derived_Tests.Test;
      Framework_Test : Framework_Tests.Test;
      Listener_Test  : Basic_Listener_Tests.Test;
      Results_Test   : Results_Tests.Test;
      Static_Test    : Static_Test_Case_Tests.Test;
      SList_Test     : SList_Tests.Test;
   begin
      Framework.Add_Static_Test (S, Assertion_Test);
      Framework.Add_Static_Test (S, Derived_Test);
      Framework.Add_Static_Test (S, Framework_Test);
      Framework.Add_Static_Test (S, Listener_Test);
      Framework.Add_Static_Test (S, Results_Test);
      Framework.Add_Static_Test (S, Static_Test);
      Framework.Add_Static_Test (S, SList_Test);
      return S;
   end Get_Test_Suite;
end Ahven_Tests;
