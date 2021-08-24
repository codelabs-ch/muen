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

with Ada.Unchecked_Conversion;

with System;

with LSC.SHA256;

package body Init.Addrspace
is

   -------------------------------------------------------------------------

   function Calculate_Hash
     (Source : Musinfo.Memregion_Type)
      return Musinfo.Hash_Type
   is
      Bytes_Per_Block : constant := LSC.SHA256.Block_Size / 8;

      subtype Block_Range is Interfaces.Unsigned_64 range
        0 .. Source.Size / Bytes_Per_Block - 1;

      type Data_Array is array (Block_Range) of LSC.SHA256.Block_Type
      with Pack;

      pragma Warnings
        (GNATprove, Off,
         "indirect writes to * through a potential alias are ignored",
         Reason => "Access to source memory region should only be granted "
         & "within the scheduling group.");
      Src_Memory : constant Data_Array
        with
          Import,
          Address => System'To_Address (Source.Address);
      pragma Annotate
        (GNATprove, Intentional,
         "object is unsuitable for aliasing via address clause",
         "Memory size information in Sinfo is generated at integration time"
          & "based on policy.");
      pragma Warnings
        (GNATprove, On,
         "indirect writes to * through a potential alias are ignored");

      --  Convert SHA256 hash to musinfo hash.
      function To_Musinfo_Hash is new Ada.Unchecked_Conversion
        (Source => LSC.SHA256.SHA256_Hash_Type,
         Target => Musinfo.Hash_Type);

      Ctx : LSC.SHA256.Context_Type
        := LSC.SHA256.SHA256_Context_Init;
   begin
      for I in Block_Range'Range loop
         LSC.SHA256.Context_Update
           (Context => Ctx,
            Block   => Src_Memory (I));
      end loop;

      --  Enforced by policy: Input region is always dividable by block size,
      --  therefore we finalize with Null_Block.

      LSC.SHA256.Context_Finalize (Context => Ctx,
                                   Block   => LSC.SHA256.Null_Block,
                                   Length  => 0);

      declare
         LSC_H  : constant LSC.SHA256.SHA256_Hash_Type
           := LSC.SHA256.SHA256_Get_Hash (Context => Ctx);
         Result : constant Musinfo.Hash_Type
           := To_Musinfo_Hash (LSC_H);
      begin
         return Result;
      end;
   end Calculate_Hash;

   -------------------------------------------------------------------------

   procedure Memcopy
     (Source      : Musinfo.Memregion_Type;
      Destination : Musinfo.Memregion_Type)
     with SPARK_Mode => Off
   is
      subtype Region_Range is Interfaces.Unsigned_64 range 1 .. Source.Size;
      type Data_Array is array (Region_Range) of Interfaces.Unsigned_8
        with Pack;

      Src_Memory : constant Data_Array
      with
         Import,
         Address => System'To_Address (Source.Address);

      Dst_Memory : Data_Array
      with
         Import,
         Address => System'To_Address (Destination.Address);
   begin
      for I in Region_Range loop
         Dst_Memory (I) := Src_Memory (I);
      end loop;
   end Memcopy;

   -------------------------------------------------------------------------

   procedure Memset
     (Region  : Musinfo.Memregion_Type;
      Pattern : Musinfo.Pattern_Type)
     with SPARK_Mode => Off
   is
      subtype Region_Range is Interfaces.Unsigned_64 range 1 .. Region.Size;

      type Data_Array is array (Region_Range) of Interfaces.Unsigned_8
        with Pack;

      Raw_Memory : Data_Array
      with
         Import,
         Address => System'To_Address (Region.Address);
   begin
      for Byte of Raw_Memory loop
         Byte := Interfaces.Unsigned_8 (Pattern);
      end loop;
   end Memset;

end Init.Addrspace;
