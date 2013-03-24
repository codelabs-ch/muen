------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Sockets;

with Unicode;
with Unicode.CES;

package Input_Sources.Socket is

   type Socket_Input is new Input_Source with private;
   type Socket_Input_Access is access all Socket_Input'Class;
   --  A special implementation of a reader, that reads from a
   --  streaming socket.
   --  Compared to Input_Sources.Html, this package does not expect to read
   --  the whole stream when calling Open. It is in fact an example on how to
   --  detect incomplete input (as opposed to invalid input).

   procedure Open
     (Socket : GNAT.Sockets.Socket_Type; Input : out Socket_Input);
   --  Open a new input reading from the socket

   procedure Close (Input : in out Socket_Input);
   --  Free the memory

   procedure Next_Char
     (From : in out Socket_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the buffer.
   --  This is a blocking procedure until some character becomes available on
   --  the socket.

   function Eof (From : Socket_Input) return Boolean;
   --  True if the socket is closed and all data received from it has been
   --  read.

private

   type Socket_Input is new Input_Source with record
      Socket      : GNAT.Sockets.Socket_Type;
      Index       : Natural;
      Buffer_Last : Natural;
      Buffer      : Unicode.CES.Byte_Sequence_Access;
      End_Of_File : Boolean;
   end record;

end Input_Sources.Socket;
