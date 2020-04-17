--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Muchannel.Writer
is

   -------------------------------------------------------------------------

   procedure Deactivate (Channel : in out Channel_Type)
   is
   begin
      Channel.Header.Epoch := Null_Epoch;
   end Deactivate;

   -------------------------------------------------------------------------

   procedure Initialize
     (Channel : out Channel_Type;
      Epoch   :     Header_Field_Type)
   is
      Local_Null_Element : Element_Type;
      for Local_Null_Element'Address use Null_Element'Address;
   begin

      --  Deactivate previous epoch.

      Channel.Header.Epoch := Null_Epoch;

      --  Initialize channel header.

      Channel.Header.Transport := SHMStream_Marker;
      Channel.Header.Protocol  := Header_Field_Type (Protocol);
      Channel.Header.Size      := Element_Size;
      Channel.Header.Elements  := Header_Field_Type (Elements);
      Channel.Header.Reserved  := 0;
      Channel.Header.WSC       := 0;
      Channel.Header.WC        := 0;

      --  Initialize channel data.

      for I in Data_Type'Range loop
         Channel.Data (I) := Local_Null_Element;
      end loop;

      --  Initiate new epoch.

      Channel.Header.Epoch := Epoch;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write
     (Channel : in out Channel_Type;
      Element :        Element_Type)
   is
      WC       : Header_Field_Type;
      Position : Data_Range;
   begin
      WC       := Channel.Header.WC;
      Position := Data_Range (WC mod Header_Field_Type (Elements));
      WC       := WC + 1;

      Channel.Header.WSC      := WC;
      Channel.Data (Position) := Element;
      Channel.Header.WC       := WC;
   end Write;

end Muchannel.Writer;
