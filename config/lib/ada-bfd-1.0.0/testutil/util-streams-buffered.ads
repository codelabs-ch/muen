-----------------------------------------------------------------------
--  Util.Streams.Buffered -- Buffered streams Stream utilities
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;
package Util.Streams.Buffered is

   pragma Preelaborate;

   --  -----------------------
   --  Buffered stream
   --  -----------------------
   --  The <b>Buffered_Stream</b> is an output/input stream which uses
   --  an intermediate buffer.  It can be configured to read or write to
   --  another stream that it will read or write using the buffer.
   --
   --  It is necessary to call <b>Flush</b> to make sure the data
   --  is written to the target stream.  The <b>Flush</b> operation will
   --  be called when finalizing the buffered stream.
   type Buffered_Stream is limited new Output_Stream and Input_Stream with private;

   type Buffer_Access is access Ada.Streams.Stream_Element_Array;

   --  Initialize the stream to read or write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Output  : in Output_Stream_Access;
                         Input   : in Input_Stream_Access;
                         Size    : in Positive);

   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Size    : in Positive);

   --  Initialize the stream to read from the string.
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Content : in String);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Buffered_Stream);

   --  Get the direct access to the buffer.
   function Get_Buffer (Stream : in Buffered_Stream) return Buffer_Access;

   --  Write a raw character on the stream.
   procedure Write (Stream : in out Buffered_Stream;
                    Char   : in Character);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in String);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Write the buffer array to the output stream.
   procedure Write (Stream : in out Buffered_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   procedure Flush (Stream : in out Buffered_Stream);

   --  Get the number of element in the stream.
   function Get_Size (Stream : in Buffered_Stream) return Natural;

   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   procedure Fill (Stream : in out Buffered_Stream);

   --  Read one character from the input stream.
   procedure Read (Stream : in out Buffered_Stream;
                   Char   : out Character);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Buffered_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Buffered_Stream;
                   Into   : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Returns True if the end of the stream is reached.
   function Is_Eof (Stream : in Buffered_Stream) return Boolean;

private

   use Ada.Streams;

   type Buffered_Stream is new Ada.Finalization.Limited_Controlled
     and Output_Stream and Input_Stream with record
      --  The buffer where the data is written before being flushed.
      Buffer      : Buffer_Access := null;

      --  The next write position within the buffer.
      Write_Pos   : Stream_Element_Offset := 0;

      --  The next read position within the buffer.
      Read_Pos    : Stream_Element_Offset := 1;

      --  The last valid write position within the buffer.
      Last        : Stream_Element_Offset := 0;

      --  The output stream to use for flushing the buffer.
      Output      : Output_Stream_Access := null;

      --  The input stream to use to fill the buffer.
      Input       : Input_Stream_Access := null;

      No_Flush    : Boolean := False;

      --  Reached end of file when reading.
      Eof         : Boolean := False;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Buffered_Stream);

end Util.Streams.Buffered;
