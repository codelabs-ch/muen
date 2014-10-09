--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
   begin

      --  Deactivate previous epoch.

      Channel.Header.Epoch := Null_Epoch;

      --  Initialize channel header.

      Channel.Header.Transport := SHMStream_Marker;
      Channel.Header.Protocol  := Writer.Protocol;
      Channel.Header.Size      := Element_Type'Size / 8;
      Channel.Header.Elements  := Header_Field_Type (Elements);
      Channel.Header.Reserved  := 0;
      Channel.Header.WSC       := 0;
      Channel.Header.WC        := 0;

      --  Initialize channel data.

      for I in Data_Type'Range loop
         Channel.Data (I) := Null_Element;
      end loop;

      --  Initiate new epoch.

      Channel.Header.Epoch := Epoch;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write
     (Channel : in out Channel_Type;
      Element :        Element_Type)
   is
      Element_Count : constant Header_Field_Type := Channel.Header.Elements;
      WC            : Header_Field_Type;
      Position      : Data_Range;
   begin
      WC       := Channel.Header.WC;
      Position := Data_Range (WC mod Element_Count);
      WC       := WC + 1;

      Channel.Header.WSC      := WC;
      Channel.Data (Position) := Element;
      Channel.Header.WC       := WC;
   end Write;

end Muchannel.Writer;
