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

package body Channels
is

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

end Channels;
