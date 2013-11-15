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

package body Channels
is

   Channel_1_In : VT_Channel.Channel_Type;
   for Channel_1_In'Address use System'To_Address (16#40000#);

   Channel_1_Reader : VT_Channel_Rdr.Reader_Type;

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

      Res := VT_Channel_Rdr.Inactive;

      loop
         VT_Channel_Rdr.Synchronize (Channel => Channel_1_In,
                                     Reader  => Channel_1_Reader,
                                     Result  => Res);
         exit when Res /= VT_Channel_Rdr.Inactive;
      end loop;
   end Init;

   -------------------------------------------------------------------------

   procedure Read
     (Char   : out Character;
      Result : out VT_Channel_Rdr.Result_Type)
   is
   begin
      VT_Channel_Rdr.Read (Channel => Channel_1_In,
                           Reader  => Channel_1_Reader,
                           Element => Char,
                           Result  => Result);
   end Read;

   -------------------------------------------------------------------------

   procedure Synchronize
   is
      Res : VT_Channel_Rdr.Result_Type;
      pragma Unreferenced (Res);
   begin
      VT_Channel_Rdr.Synchronize (Channel => Channel_1_In,
                                  Reader  => Channel_1_Reader,
                                  Result  => Res);
   end Synchronize;

   -------------------------------------------------------------------------

   procedure Write (Char : Character)
   is
   begin
      VT_Channel_Wtr.Write (Channel => Channel_1_Out,
                            Element => Char);
   end Write;

end Channels;
