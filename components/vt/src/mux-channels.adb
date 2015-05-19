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

with System;

with SK.Hypercall;

package body Mux.Channels
is

   use VT_Channels;

   type In_Channel_Array is array
     (Input_Channel_Range) of VT_Channel.Channel_Type;

   type In_Reader_Array is array
     (Input_Channel_Range) of VT_Channel_Rdr.Reader_Type;

   In_Channels : In_Channel_Array;
   for In_Channels'Address use System'To_Address (16#100000#);

   In_Readers : In_Reader_Array := (others => VT_Channel_Rdr.Null_Reader);

   type Out_Channel_Array is array
     (Output_Channel_Range) of Input_Event_Channel.Channel_Type;

   Out_Channels : Out_Channel_Array;
   for Out_Channels'Address use System'To_Address (16#50000#);

   -------------------------------------------------------------------------

   function Has_Pending_Data (Channel : Input_Channel_Range) return Boolean
   is
      Result : Boolean;
   begin
      VT_Channel_Rdr.Has_Pending_Data
        (Channel => In_Channels (Channel),
         Reader  => In_Readers (Channel),
         Result  => Result);
      return Result;
   end Has_Pending_Data;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      for Channel of Out_Channels loop
         Input_Event_Channel_Wtr.Initialize
           (Channel => Channel,
            Epoch   => 1);
      end loop;
   end Init;

   -------------------------------------------------------------------------

   procedure Read
     (Channel :     Input_Channel_Range;
      Char    : out Character;
      Result  : out VT_Channel_Rdr.Result_Type)
   is
   begin
      VT_Channel_Rdr.Read (Channel => In_Channels (Channel),
                           Reader  => In_Readers (Channel),
                           Element => Char,
                           Result  => Result);
   end Read;

   -------------------------------------------------------------------------

   procedure Write
     (Channel : Output_Channel_Range;
      Event   : Input.Key_Event_Type)
   is
   begin
      Input_Event_Channel_Wtr.Write
        (Channel => Out_Channels (Channel),
         Element => Event);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Channel));
   end Write;

end Mux.Channels;
