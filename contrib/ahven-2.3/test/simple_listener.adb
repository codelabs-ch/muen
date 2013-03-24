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

package body Simple_Listener is
   procedure Add_Pass (Object : in out Listener;
                       Info : Ahven.Listeners.Context) is
   begin
      Object.Passes := Object.Passes + 1;
   end Add_Pass;

   procedure Add_Failure (Object : in out Listener;
                          Info : Ahven.Listeners.Context) is
   begin
      Object.Failures := Object.Failures + 1;
   end Add_Failure;

   procedure Add_Skipped (Object: in out Listener;
                          Info : Ahven.Listeners.Context) is
   begin
      Object.Skips := Object.Skips + 1;
   end Add_Skipped;

   procedure Add_Error (Object : in out Listener;
                        Info : Ahven.Listeners.Context) is
   begin
      Object.Errors := Object.Errors + 1;
   end Add_Error;

   procedure Start_Test (Object : in out Listener;
                         Info : Ahven.Listeners.Context) is
   begin
      Object.Level := Object.Level + 1;
      Object.Start_Calls := Object.Start_Calls + 1;
   end Start_Test;

   procedure End_Test (Object : in out Listener;
                       Info : Ahven.Listeners.Context) is
   begin
      Object.Level := Object.Level - 1;
      Object.End_Calls := Object.End_Calls + 1;
   end End_Test;
end Simple_Listener;
