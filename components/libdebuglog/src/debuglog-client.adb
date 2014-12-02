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

with Debuglog.Utils;
with Debuglog.Sink;

package body Debuglog.Client
is

   type Word64_Image_Index is range 0 .. 15;
   type Word64_Image is array (Word64_Image_Index) of Character;

   type Word32_Image_Index is range 0 .. 7;
   type Word32_Image is array (Word32_Image_Index) of Character;

   type Word16_Image_Index is range 0 .. 3;
   type Word16_Image is array (Word16_Image_Index) of Character;

   type Byte_Image_Index is range 0 .. 1;
   type Byte_Image is array (Byte_Image_Index) of Character;

   -------------------------------------------------------------------------

   procedure Flush renames Sink.Flush;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Put (Item => Character'Val (16#0d#)); -- CR
      Put (Item => Character'Val (16#0a#)); -- LF
      Sink.Flush;
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

   procedure Put_Byte (Item : Byte)
   is
      use Debuglog.Utils;
      use type Interfaces.Unsigned_8;

      Temp  : Byte;
      Image : Byte_Image;
   begin
      Temp := Item;
      for I in reverse Image'Range loop
         Image (I) := Num_To_Char (Value => Nibble_Type (Temp mod 16));
         Temp := Temp / 16;
      end loop;

      for I in Image'Range loop
         Put (Item => Image (I));
      end loop;
   end Put_Byte;

   -------------------------------------------------------------------------

   procedure Put_If
     (Condition : Boolean;
      Item      : String)
   is
   begin
      if Condition then
         Put_Line (Item => Item);
      end if;
   end Put_If;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String)
   is
   begin
      Put (Item => Item);
      New_Line;
   end Put_Line;

   -------------------------------------------------------------------------

   procedure Put_Reg8
     (Name  : String;
      Value : Byte)
   is
   begin
      Put      (Item => Name);
      Put      (Item => ": 16#");
      Put_Byte (Item => Value);
      Put_Line (Item => "#");
   end Put_Reg8;

   -------------------------------------------------------------------------

   procedure Put_Reg16
     (Name  : String;
      Value : Word16)
   is
   begin
      Put        (Item => Name);
      Put        (Item => ": 16#");
      Put_Word16 (Item => Value);
      Put_Line   (Item => "#");
   end Put_Reg16;

   -------------------------------------------------------------------------

   procedure Put_Reg32
     (Name  : String;
      Value : Word32)
   is
   begin
      Put        (Item => Name);
      Put        (Item => ": 16#");
      Put_Word32 (Item => Value);
      Put_Line   (Item => "#");
   end Put_Reg32;

   -------------------------------------------------------------------------

   procedure Put_Reg64
     (Name  : String;
      Value : Word64)
   is
   begin
      Put        (Item => Name);
      Put        (Item => ": 16#");
      Put_Word64 (Item => Value);
      Put_Line   (Item => "#");
   end Put_Reg64;

   -------------------------------------------------------------------------

   procedure Put_UInt64 (Item : Word64)
   is
      use Debuglog.Utils;
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;

      Temp  : Word64;
      Image : Word64_Image := Word64_Image'(others => '_');
   begin
      Temp := Item;
      for I in reverse Image'Range loop
         Image (I) := Num_To_Char (Value => Nibble_Type (Temp mod 10));
         Temp := Temp / 10;
         if Temp = 0 then
            exit;
         end if;
      end loop;

      for I in Image'Range loop
         if Image (I) /= '_' then
            Put (Item => Image (I));
         end if;
      end loop;
   end Put_UInt64;

   -------------------------------------------------------------------------

   procedure Put_Word16 (Item : Word16)
   is
      use Debuglog.Utils;
      use type Interfaces.Unsigned_16;

      Temp  : Word16;
      Image : Word16_Image;
   begin
      Temp := Item;
      for I in reverse Image'Range loop
         Image (I) := Num_To_Char (Value => Nibble_Type (Temp mod 16));
         Temp := Temp / 16;
      end loop;

      for I in Image'Range loop
         Put (Item => Image (I));
      end loop;
   end Put_Word16;

   -------------------------------------------------------------------------

   procedure Put_Word32 (Item : Word32)
   is
      use Debuglog.Utils;
      use type Interfaces.Unsigned_32;

      Temp  : Word32;
      Image : Word32_Image;
   begin
      Temp := Item;
      for I in reverse Image'Range loop
         Image (I) := Num_To_Char (Value => Nibble_Type (Temp mod 16));
         Temp := Temp / 16;
      end loop;

      for I in Image'Range loop
         Put (Item => Image (I));
      end loop;
   end Put_Word32;

   -------------------------------------------------------------------------

   procedure Put_Word64 (Item : Word64)
   is
      use Debuglog.Utils;
      use type Interfaces.Unsigned_64;

      Temp  : Word64;
      Image : Word64_Image;
   begin
      Temp := Item;
      for I in reverse Image'Range loop
         Image (I) := Num_To_Char (Value => Nibble_Type (Temp mod 16));
         Temp := Temp / 16;
      end loop;

      for I in Image'Range loop
         Put (Item => Image (I));
      end loop;
   end Put_Word64;

end Debuglog.Client;
