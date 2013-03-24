-----------------------------------------------------------------------
--  Util.Streams -- Stream utilities
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

with Ada.Streams;
package Util.Streams is

   pragma Preelaborate;

   --  -----------------------
   --  Output stream
   --  -----------------------
   --  The <b>Output_Stream</b> is an interface that accepts output bytes
   --  and sends them to a sink.
   type Output_Stream is limited interface;
   type Output_Stream_Access is access all Output_Stream'Class;

   --  Write the buffer array to the output stream.
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is abstract;

   --  Flush the buffer (if any) to the sink.
   procedure Flush (Stream : in out Output_Stream) is null;

   --  Close the sink.
   procedure Close (Stream : in out Output_Stream) is null;

   --  -----------------------
   --  Input stream
   --  -----------------------
   --  The <b>Input_Stream</b> is the interface that reads input bytes
   --  from a source and returns them.
   type Input_Stream is limited interface;
   type Input_Stream_Access is access all Input_Stream'Class;

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Input_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is abstract;

   --  Copy the input stream to the output stream until the end of the input stream
   --  is reached.
   procedure Copy (From : in out Input_Stream'Class;
                   Into : in out Output_Stream'Class);

   --  Notes:
   --  ------
   --  The <b>Ada.Streams.Root_Stream_Type</b> implements the <b>Output_Stream</b>
   --  and <b>Input_Stream</b>.  It is however not easy to use for composing various
   --  stream behaviors.
end Util.Streams;
