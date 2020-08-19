--
--  Copyright (C) 2014  secunet Security Networks AG
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

private with Debuglog.Sink;

package body Debuglog.Client
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Flush renames Sink.Flush;

   -------------------------------------------------------------------------

   procedure Init (Epoch : Interfaces.Unsigned_64) renames Sink.Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Put (Item => Character'Val (16#0a#)); -- LF
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put (Item : Character)
   is
   begin
      Sink.Write_Character (Item => Item);
   end Put;

   -------------------------------------------------------------------------

   procedure Put (Item : String)
   is
   begin
      for I in Item'Range loop
         Put (Item => Item (I));
      end loop;
   end Put;

   -------------------------------------------------------------------------

   procedure Put (Item : Boolean)
   is
   begin
      if Item then
         Put (Item => "true");
      else
         Put (Item => "false");
      end if;
   end Put;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String)
   is
   begin
      Put (Item => Item);
      New_Line;
   end Put_Line;

end Debuglog.Client;
