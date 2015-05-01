--
--  Copyright (C) 2014-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

package body Musinfo.Writer
is

   use type Ada.Streams.Stream_Element_Offset;

   subtype Subject_Info_Stream is Ada.Streams.Stream_Element_Array
     (1 .. Subject_Info_Type_Size);

   --  Subject_Info_Type'Write adds additional output so manual conversion to
   --  a Stream_Element_Array is necessary.
   function Convert is new Ada.Unchecked_Conversion
     (Source => Subject_Info_Type,
      Target => Subject_Info_Stream);

   -------------------------------------------------------------------------

   procedure Serialize
     (Info     : Subject_Info_Type;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create
        (File => File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Filename);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Convert (S => Info));
      Ada.Streams.Stream_IO.Close (File => File);
   end Serialize;

end Musinfo.Writer;
