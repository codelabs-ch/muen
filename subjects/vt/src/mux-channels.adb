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

package body Mux.Channels
is

   type In_Channel_Array is array
     (Input_Channel_Range) of VT_Channel.Channel_Type;

   type In_Reader_Array is array
     (Input_Channel_Range) of VT_Channel_Rdr.Reader_Type;

   In_Channels : In_Channel_Array;
   for In_Channels'Address use (System'To_Address (16#40000#));

   In_Readers : In_Reader_Array;

   Channel_1_Out : VT_Channel.Channel_Type;
   for Channel_1_Out'Address use System'To_Address (16#50000#);

   -------------------------------------------------------------------------

   procedure Init
   is
      use type VT_Channel_Rdr.Result_Type;

      Res : VT_Channel_Rdr.Result_Type;
   begin
      VT_Channel_Wtr.Initialize (Channel => Channel_1_Out,
                                 Epoch   => 1);

      for I in In_Channels'Range loop
         Res := VT_Channel_Rdr.Inactive;

         loop
            VT_Channel_Rdr.Synchronize (Channel => In_Channels (I),
                                        Reader  => In_Readers (I),
                                        Result  => Res);
            exit when Res /= VT_Channel_Rdr.Inactive;
         end loop;
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

   procedure Synchronize (Channel : Input_Channel_Range)
   is
      Res : VT_Channel_Rdr.Result_Type;
      pragma Unreferenced (Res);
   begin
      VT_Channel_Rdr.Synchronize (Channel => In_Channels (Channel),
                                  Reader  => In_Readers (Channel),
                                  Result  => Res);
   end Synchronize;

   -------------------------------------------------------------------------

   procedure Write (Char : Character)
   is
   begin
      VT_Channel_Wtr.Write (Channel => Channel_1_Out,
                            Element => Char);
   end Write;

end Mux.Channels;
