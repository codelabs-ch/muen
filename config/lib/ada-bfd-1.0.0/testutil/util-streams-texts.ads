-----------------------------------------------------------------------
--  Util.Streams.Files -- File Stream utilities
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
with Util.Streams.Buffered;
with Util.Texts.Transforms;
with Ada.Characters.Handling;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;
package Util.Streams.Texts is

   --  -----------------------
   --  Print stream
   --  -----------------------
   --  The <b>Print_Stream</b> is an output stream which provides helper methods
   --  for writing text streams.
   type Print_Stream is new Buffered.Buffered_Stream with private;
   type Print_Stream_Access is access all Print_Stream'Class;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Output_Stream_Access);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Long_Long_Integer);

   --  Write a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a date on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Calendar.Time;
                    Format : in GNAT.Calendar.Time_IO.Picture_String
                    := GNAT.Calendar.Time_IO.ISO_Date);

   --  Get the output stream content as a string.
   function To_String (Stream : in Buffered.Buffered_Stream) return String;


   package TR is
     new Util.Texts.Transforms (Stream => Buffered.Buffered_Stream,
                                Char   => Character,
                                Input  => String,
                                Put    => Buffered.Write,
                                To_Upper => Ada.Characters.Handling.To_Upper,
                                To_Lower => Ada.Characters.Handling.To_Lower,
                                To_Input => To_String);

   --  -----------------------
   --  Reader stream
   --  -----------------------
   --  The <b>Reader_Stream</b> is an input stream which provides helper methods
   --  for reading text streams.
   type Reader_Stream is new Buffered.Buffered_Stream with private;
   type Reader_Stream_Access is access all Reader_Stream'Class;

   --  Initialize the reader to read the input from the input stream given in <b>From</b>.
   procedure Initialize (Stream : in out Reader_Stream;
                         From   : in Input_Stream_Access);

   --  Read an input line from the input stream.  The line is terminated by ASCII.LF.
   --  When <b>Strip</b> is set, the line terminators (ASCII.CR, ASCII.LF) are removed.
   procedure Read_Line (Stream : in out Reader_Stream;
                        Into   : out Ada.Strings.Unbounded.Unbounded_String;
                        Strip  : in Boolean := False);


private

   type Print_Stream is new Buffered.Buffered_Stream with null record;

   type Reader_Stream is new Buffered.Buffered_Stream with null record;

end Util.Streams.Texts;
