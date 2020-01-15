--
--  Copyright (C) 2016  secunet Security Networks AG
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

with HW.DbC;

with Dbg.Byte_Arrays;
with Dbg.Non_Interfering_Output;

package body Dbg.Xhci_Dbg
is

   --  Receive data up until Length bytes from xHCI DbC into given buffer. The
   --  actual number of bytes received is return by Length.
   procedure Receive_Buffer
     (Buffer : in out Byte_Arrays.Byte_Array;
      Length : in out Byte_Arrays.Byte_Array_Range);

   --  Send data in buffer up until Length bytes to xHCI DbC. The actual number
   --  of bytes sent is returned by Length.
   procedure Send_Buffer
     (Buffer  :        Byte_Arrays.Byte_Array;
      Length  : in out Byte_Arrays.Byte_Array_Range;
      Success :    out Boolean);

   package NIO is new Non_Interfering_Output
     (Buffer_Size => 4096,
      Receive     => Receive_Buffer,
      Send        => Send_Buffer);

   -------------------------------------------------------------------------

   procedure Init renames HW.DbC.Init;

   -------------------------------------------------------------------------

   procedure Receive_Buffer
     (Buffer : in out Byte_Arrays.Byte_Array;
      Length : in out Byte_Arrays.Byte_Array_Range)
   is
   begin
      HW.DbC.Receive (Buf => HW.Buffer (Buffer),
                      Len => Length);
   end Receive_Buffer;

   -------------------------------------------------------------------------

   procedure Send_Buffer
     (Buffer  :        Byte_Arrays.Byte_Array;
      Length  : in out Byte_Arrays.Byte_Array_Range;
      Success :    out Boolean)
   is
   begin
      HW.DbC.Send (Buf     => HW.Buffer (Buffer),
                   Len     => Length,
                   Success => Success);
   end Send_Buffer;

   -------------------------------------------------------------------------

   procedure Run
     (Console      : in out Consoles.Console_Type;
      Input_Queue  : in out Byte_Queue.Queue_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
      renames NIO.Run;

end Dbg.Xhci_Dbg;
