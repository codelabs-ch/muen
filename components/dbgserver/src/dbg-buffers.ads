--
--  Copyright (C) 2014  secunet Security Networks AG
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

with Debuglog.Types;
with Debuglog.Stream.Reader;

with Dbg.Byte_Queue;

private package Dbg.Buffers
is

   --  Subject log buffers.
   type Buffer_Type is limited private;

   --  Initialize subject log buffers.
   procedure Initialize (Buffer : out Buffer_Type);

   --  Run buffers.
   procedure Run
      (Buffer       : in out Buffer_Type;
       Output_Queue : in out Byte_Queue.Queue_Type);

private

   type Subject_Buffer_Type is record
      Cache              : Debuglog.Types.Data_Type;
      State              : Debuglog.Stream.Reader.Reader_Type;
      Message_Incomplete : Boolean;
      Overrun_Occurred   : Boolean;
      New_Epoch_Occurred : Boolean;
      Enabled            : Boolean;
   end record;

   type Subject_Buffers_Type is array (Subject_Buffer_Range)
     of Subject_Buffer_Type;

   type Buffer_Type is record
      Subjects     : Subject_Buffers_Type;
      Is_Idle      : Boolean;
      Last_Subject : Subject_Buffer_Range;
   end record;

end Dbg.Buffers;
