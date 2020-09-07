--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

package Mucontrol.Command
is

   type Command_Type is new Interfaces.Unsigned_64
     with Size => 64;

   --  Do nothing.
   CMD_NOP       : constant Command_Type := 16#0000#;
   --  Synchronize state with command page.
   CMD_SYNC      : constant Command_Type := 16#0001#;
   --  Erase writable memory regions.
   CMD_ERASE     : constant Command_Type := 16#0002#;
   --  Prepare writable memory regions by setting initial content.
   CMD_PREPARE   : constant Command_Type := 16#0003#;
   --  Validate all memory regions
   CMD_VALIDATE  : constant Command_Type := 16#0004#;
   --  Run component
   CMD_RUN       : constant Command_Type := 16#0005#;
   --  Subject is self-governed, no need to wait & report status.
   CMD_SELF_CTRL : constant Command_Type := 16#FFFF_FFFF_FFFF_FFFF#;

   --  Watchdog is disabled.
   WD_DISABLED : constant Interfaces.Unsigned_64 := 16#FFFF_FFFF_FFFF_FFFF#;

   Padding_Start_Byte : constant := 8 + 8 + 8;
   Padding_Size       : constant := (Page_Size - Padding_Start_Byte) * 8;

   type Padding_Type is array
     (Padding_Start_Byte .. Page_Size - 1) of Interfaces.Unsigned_8
     with Size => Padding_Size;

   type Command_Interface_Type is record
      Command           : Command_Type with Atomic;
      Epoch             : Interfaces.Unsigned_64;
      Watchdog_Interval : Interfaces.Unsigned_64;
      Reserved          : Padding_Type;
   end record
     with
       Object_Size => Page_Size * 8,
       Size        => Page_Size * 8,
       Volatile;

   for Command_Interface_Type use record
      Command           at  0 range 0 .. 63;
      Epoch             at  8 range 0 .. 63;
      Watchdog_Interval at 16 range 0 .. 63;
      Reserved          at Padding_Start_Byte range 0 .. Padding_Size - 1;
   end record;

end Mucontrol.Command;
