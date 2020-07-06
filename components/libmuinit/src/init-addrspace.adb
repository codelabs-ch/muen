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

with System;

package body Init.Addrspace
is

   -------------------------------------------------------------------------

   procedure Memcopy
     (Source      : Musinfo.Memregion_Type;
      Destination : Musinfo.Memregion_Type)
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
