--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

generic
package Muchannel.Readers
is

   type Result_Type is
     (Inactive,
      Incompatible_Interface,
      Epoch_Changed,
      No_Data,
      Overrun_Detected,
      Success);

   type Reader_Type is private;

   --  Returns True if the channel is active and has pending data to be read.
   procedure Has_Pending_Data
     (Channel :     Channel_Type;
      Reader  :     Reader_Type;
      Result  : out Boolean)
   with
      Global  => null,
      Depends => (Result => (Channel, Reader));

   --  Read next element from given channel.
   procedure Read
     (Channel :        Channel_Type;
      Reader  : in out Reader_Type;
      Element :    out Element_Type;
      Result  :    out Result_Type)
   with
      Global  => null,
      Depends => ((Reader, Element, Result) => (Channel, Reader));

   --  Drain all current channel elements.
   procedure Drain
     (Channel :        Channel_Type;
      Reader  : in out Reader_Type)
   with
      Global  => null,
      Depends => (Reader =>+ Channel);

   --  Reset reader element index.
   procedure Reset (Reader : in out Reader_Type)
   with
      Global  => null,
      Depends => (Reader => Reader);

   Null_Reader : constant Reader_Type;

private

   type Reader_Type is record
      Epoch    : Header_Field_Type;
      Protocol : Header_Field_Type;
      Size     : Header_Field_Type;
      Elements : Header_Field_Type;
      RC       : Header_Field_Type;
   end record;

   Null_Reader : constant Reader_Type
     := Reader_Type'(Epoch    => Null_Epoch,
                     Protocol => Header_Field_Type'First,
                     Size     => Header_Field_Type'First,
                     Elements => Header_Field_Type'First,
                     RC       => Header_Field_Type (Data_Range'First));

end Muchannel.Readers;
