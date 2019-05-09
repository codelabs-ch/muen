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

with Input.Event_Channel.Writer_Instance;

package body Mux.Channels
is

   use VT_Channels;

   type In_Channel_Array is array
     (Input_Channel_Range) of VT_Channel.Channel_Type
     with
       Component_Size => Cspecs.Console_Element_Size * 8;

   type In_Reader_Array is array
     (Input_Channel_Range) of VT_Channel_Rdr.Reader_Type;

   In_Channels : In_Channel_Array
     with
       Address => System'To_Address (Cspecs.Console_Address_Base);

   In_Readers : In_Reader_Array := (others => VT_Channel_Rdr.Null_Reader);

   type Out_Channel_Array is array
     (Output_Channel_Range) of Input.Event_Channel.Channel_Type
     with
       Component_Size => Cspecs.Input_Devices_Element_Size * 8;

   Out_Channels : Out_Channel_Array
     with
       Address => System'To_Address (Cspecs.Input_Devices_Address_Base),
       Size    => Cspecs.Input_Devices_Element_Count
         * Cspecs.Input_Devices_Element_Size * 8;

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
   with
      SPARK_Mode => Off
   is
   begin
      for Channel of Out_Channels loop
         Input.Event_Channel.Writer_Instance.Initialize
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
      Event   : Input.Input_Event_Type)
   is
   begin
      Input.Event_Channel.Writer_Instance.Write
        (Channel => Out_Channels (Channel),
         Element => Event);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Channel));
   end Write;

end Mux.Channels;
