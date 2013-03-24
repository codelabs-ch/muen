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

with Ahven.Listeners;

package Simple_Listener is
   type Listener is new Ahven.Listeners.Result_Listener with record
      Passes      : Natural := 0;
      Errors      : Natural := 0;
      Failures    : Natural := 0;
      Skips       : Natural := 0;
      Level       : Integer := 0;
      Start_Calls : Natural := 0;
      End_Calls   : Natural := 0;
   end record;

   type Listener_Access is access all Listener;

   procedure Add_Pass (Object: in out Listener;
                       Info : Ahven.Listeners.Context);

   procedure Add_Failure (Object: in out Listener;
                          Info : Ahven.Listeners.Context);

   procedure Add_Skipped (Object: in out Listener;
                          Info : Ahven.Listeners.Context);

   procedure Add_Error (Object: in out Listener;
                        Info : Ahven.Listeners.Context);

   procedure Start_Test (Object: in out Listener;
                         Info : Ahven.Listeners.Context);

   procedure End_Test (Object: in out Listener;
                       Info : Ahven.Listeners.Context);

end Simple_Listener;

