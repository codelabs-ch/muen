--
--  Copyright (C) 2018  secunet Security Networks AG
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

with Interfaces;

package Decoder
is

   --  Decode the output of the PC speaker sink of the debug server.  The
   --  procedure reads a stream of frequencies, as provided by the tool
   --  aubiopitch, from standard input.  It demodulates the stream, and outputs
   --  the decoded debug log to standard output.
   procedure Decode;

private

   subtype Byte_Type  is Interfaces.Unsigned_8;
   subtype State_Type is Byte_Type range 0 .. 8;
   subtype Phase_Type is Byte_Type range 0 .. 1;
   subtype Bit_Type   is Byte_Type range 0 .. 1;

   Decode_Exception : exception;

   Byte_Sep : constant Float := 2600.0;

   Code : constant array (Phase_Type, Bit_Type) of Float
     := (0 => (200.0, 700.0), 1 => (1100.0, 1700.0));

   Epsilon : constant Float := 20.0;

   function Next_Frequency return Float;

   function Is_Byte_Sep (Frequency : Float) return Boolean;

   function Is_Nearly (L, R : Float) return Boolean;

   procedure Symbol_From_Freq
     (Frequency :     Float;
      Found     : out Boolean;
      Phase     : out Phase_Type;
      Bit       : out Bit_Type);

end Decoder;
