-----------------------------------------------------------------------
--  Util.Streams.Buffered -- Buffered streams Stream utilities
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
package body Util.Streams.Buffered is

   procedure Free_Buffer is
     new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                     Name   => Buffer_Access);

   --  ------------------------------
   --  Initialize the stream to read or write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Output  : in Output_Stream_Access;
                         Input   : in Input_Stream_Access;
                         Size    : in Positive) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Last      := Stream_Element_Offset (Size);
      Stream.Buffer    := new Stream_Element_Array (1 .. Stream.Last);
      Stream.Output    := Output;
      Stream.Input     := Input;
      Stream.Write_Pos := 1;
      Stream.Read_Pos  := 1;
      Stream.No_Flush  := False;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read from the string.
   --  ------------------------------
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Content : in String) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Last      := Stream_Element_Offset (Content'Length);
      Stream.Buffer    := new Stream_Element_Array (1 .. Content'Length);
      Stream.Output    := null;
      Stream.Input     := null;
      Stream.Write_Pos := Stream.Last + 1;
      Stream.Read_Pos  := 1;
      Stream.No_Flush  := False;
      for I in Content'Range loop
         Stream.Buffer (Stream_Element_Offset (I - Content'First + 1))
           := Character'Pos (Content (I));
      end loop;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   --  ------------------------------
   procedure Initialize (Stream  : in out Buffered_Stream;
                         Size    : in Positive) is
   begin
      Stream.Initialize (Output => null, Input => null, Size => Size);
      Stream.No_Flush := True;
      Stream.Read_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Close the sink.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Buffered_Stream) is
   begin
      if Stream.Output /= null then
         Buffered_Stream'Class (Stream).Flush;
         Stream.Output.Close;
      end if;
   end Close;

   --  ------------------------------
   --  Get the direct access to the buffer.
   --  ------------------------------
   function Get_Buffer (Stream : in Buffered_Stream) return Buffer_Access is
   begin
      return Stream.Buffer;
   end Get_Buffer;

   --  ------------------------------
   --  Get the number of element in the stream.
   --  ------------------------------
   function Get_Size (Stream : in Buffered_Stream) return Natural is
   begin
      return Natural (Stream.Write_Pos - Stream.Read_Pos);
   end Get_Size;

   --  ------------------------------
   --  Write a raw character on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Buffered_Stream;
                    Char   : in Character) is
   begin
      if Stream.Write_Pos > Stream.Last then
         Stream.Flush;
         if Stream.Write_Pos > Stream.Last then
            raise Ada.IO_Exceptions.End_Error with "Buffer is full";
         end if;
      end if;
      Stream.Buffer (Stream.Write_Pos) := Stream_Element (Character'Pos (Char));
      Stream.Write_Pos := Stream.Write_Pos + 1;
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in String) is
      Start : Positive := Item'First;
      Pos   : Stream_Element_Offset := Stream.Write_Pos;
      Avail : Natural;
      Size  : Natural;
      Char  : Character;
   begin
      while Start <= Item'Last loop
         Size := Item'Last - Start + 1;
         Avail := Natural (Stream.Last - Pos + 1);
         if Avail = 0 then
            Stream.Flush;
            Pos := Stream.Write_Pos;
            Avail := Natural (Stream.Last - Pos + 1);
            if Avail = 0 then
               raise Ada.IO_Exceptions.End_Error with "Buffer is full";
            end if;
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         while Size > 0 loop
            Char := Item (Start);
            Stream.Buffer (Pos) := Stream_Element (Character'Pos (Char));
            Pos := Pos + 1;
            Start := Start + 1;
            Size := Size - 1;
         end loop;
         Stream.Write_Pos := Pos;
      end loop;
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
      Count : constant Natural := Ada.Strings.Unbounded.Length (Item);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Stream.Write (Char => Ada.Strings.Unbounded.Element (Item, I));
         end loop;
      end if;
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Buffered_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
      Count : constant Natural := Ada.Strings.Wide_Wide_Unbounded.Length (Item);
      C     : Wide_Wide_Character;
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            C := Ada.Strings.Wide_Wide_Unbounded.Element (Item, I);
            Stream.Write (Char => Character'Val (Wide_Wide_Character'Pos (C)));
         end loop;
      end if;
   end Write;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   procedure Write (Stream : in out Buffered_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Start : Stream_Element_Offset := Buffer'First;
      Pos   : Stream_Element_Offset := Stream.Write_Pos;
      Avail : Stream_Element_Offset;
      Size  : Stream_Element_Offset;
   begin
      while Start <= Buffer'Last loop
         Size := Buffer'Last - Start + 1;
         Avail := Stream.Last - Pos + 1;
         if Avail = 0 then
            Stream.Flush;
            Pos := Stream.Write_Pos;
            Avail := Stream.Last - Pos + 1;
            if Avail = 0 then
               raise Ada.IO_Exceptions.End_Error with "Buffer is full";
            end if;
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         Stream.Buffer (Pos .. Pos + Size - 1) := Buffer (Start .. Start + Size - 1);
         Start := Start + Size;
         Pos   := Pos + Size;
         Stream.Write_Pos := Pos;

         --  If we have still more data that the buffer size, flush and write
         --  the buffer directly.
         if Start < Buffer'Last and then Buffer'Last - Start > Stream.Buffer'Length then
            Stream.Flush;
            Stream.Output.Write (Buffer (Start .. Buffer'Last));
            return;
         end if;
      end loop;
   end Write;

   --  ------------------------------
   --  Flush the stream.
   --  ------------------------------
   procedure Flush (Stream : in out Buffered_Stream) is
   begin
      if Stream.Write_Pos > 1 and not Stream.No_Flush then
         if Stream.Output = null then
            raise Ada.IO_Exceptions.Data_Error with "Output buffer is full";
         else
            Stream.Output.Write (Stream.Buffer (1 .. Stream.Write_Pos - 1));
            Stream.Output.Flush;
         end if;
         Stream.Write_Pos := 1;
      end if;
   end Flush;

   --  ------------------------------
   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   --  ------------------------------
   procedure Fill (Stream : in out Buffered_Stream) is
   begin
      if Stream.Input = null then
         Stream.Eof := True;
      else
         Stream.Input.Read (Stream.Buffer (1 .. Stream.Last - 1), Stream.Write_Pos);
         Stream.Eof := Stream.Write_Pos < 1;
         if not Stream.Eof then
            Stream.Write_Pos := Stream.Write_Pos + 1;
         end if;
         Stream.Read_Pos := 1;
      end if;
   end Fill;

   --  ------------------------------
   --  Read one character from the input stream.
   --  ------------------------------
   procedure Read (Stream : in out Buffered_Stream;
                   Char   : out Character) is
   begin
      if Stream.Read_Pos >= Stream.Write_Pos then
         Stream.Fill;
         if Stream.Eof then
            raise Ada.IO_Exceptions.Data_Error with "End of buffer";
         end if;
      end if;
      Char := Character'Val (Stream.Buffer (Stream.Read_Pos));
      Stream.Read_Pos := Stream.Read_Pos + 1;
   end Read;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  ------------------------------
   procedure Read (Stream : in out Buffered_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Start : Stream_Element_Offset := Into'First;
      Pos   : Stream_Element_Offset := Stream.Read_Pos;
      Avail : Stream_Element_Offset;
      Size  : Stream_Element_Offset;
      Total : Stream_Element_Offset := 0;
   begin
      while Start <= Into'Last loop
         Size := Into'Last - Start + 1;
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            Pos := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
            exit when Avail = 0;
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         Into (Start .. Start + Size - 1) := Stream.Buffer (Pos .. Pos + Size - 1);
         Start := Start + Size;
         Pos   := Pos + Size;
         Total := Total + Size;
         Stream.Read_Pos := Pos;
      end loop;
      Last := Total;
   end Read;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  ------------------------------
   procedure Read (Stream : in out Buffered_Stream;
                   Into   : in out Ada.Strings.Unbounded.Unbounded_String) is
      Pos   : Stream_Element_Offset := Stream.Read_Pos;
      Avail : Stream_Element_Offset;
   begin
      loop
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            if Stream.Eof then
               return;
            end if;
            Pos   := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
         end if;
         for I in 1 .. Avail loop
            Ada.Strings.Unbounded.Append (Into, Character'Val (Stream.Buffer (Pos)));
            Pos := Pos + 1;
         end loop;
         Stream.Read_Pos := Pos;
      end loop;
   end Read;

   --  ------------------------------
   --  Flush the stream and release the buffer.
   --  ------------------------------
   procedure Finalize (Object : in out Buffered_Stream) is
   begin
      if Object.Buffer /= null then
         if Object.Output /= null then
            Object.Flush;
         end if;
         Free_Buffer (Object.Buffer);
      end if;
   end Finalize;

   --  ------------------------------
   --  Returns True if the end of the stream is reached.
   --  ------------------------------
   function Is_Eof (Stream : in Buffered_Stream) return Boolean is
   begin
      return Stream.Eof;
   end Is_Eof;

end Util.Streams.Buffered;
