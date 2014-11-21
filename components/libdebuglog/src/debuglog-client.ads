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

with Interfaces;

package Debuglog.Client
is

   subtype Byte is Interfaces.Unsigned_8;
   subtype Word16 is Interfaces.Unsigned_16;
   subtype Word32 is Interfaces.Unsigned_32;
   subtype Word64 is Interfaces.Unsigned_64;

   --  Write character.
   procedure Put (Item : Character);

   --  Write string.
   procedure Put (Item : String);

   --  Write boolean.
   procedure Put (Item : Boolean);

   --  Write string and start new line.
   procedure Put_Line (Item : String);

   --  Start new line.
   procedure New_Line;

   --  Write string if condition is true.
   procedure Put_If
     (Condition : Boolean;
      Item      : String);

   --  Write Byte.
   procedure Put_Byte (Item : Byte);

   --  Write Word16.
   procedure Put_Word16 (Item : Word16);

   --  Write Word32.
   procedure Put_Word32 (Item : Word32);

   --  Write Word64.
   procedure Put_Word64 (Item : Word64);

   --  Write UInt64 as decimal value.
   procedure Put_UInt64 (Item : Word64);

   --  Write string and Byte and start new line.
   procedure Put_Reg8
     (Name  : String;
      Value : Byte);

   --  Write string and Word16 and start new line.
   procedure Put_Reg16
     (Name  : String;
      Value : Word16);

   --  Write string and Word32 and start new line.
   procedure Put_Reg32
     (Name  : String;
      Value : Word32);

   --  Write string and Word64 and start new line.
   procedure Put_Reg64
     (Name  : String;
      Value : Word64);

   --  Flush buffers.
   procedure Flush;

end Debuglog.Client;
