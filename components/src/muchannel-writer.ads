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

generic

   --  Protocol identifier.
   Protocol : Muchannel.Header_Field_Type;

   --  Null element used for channel initialization.
   Null_Element : Muchannel.Element_Type;

package Muchannel.Writer
is

   --  Initialize channel with given epoch.
   procedure Initialize
     (Channel : out Channel_Type;
      Epoch   :     Header_Field_Type);

   --  Deactivate channel.
   procedure Deactivate (Channel : in out Channel_Type);

   --  Write element to given channel.
   procedure Write
     (Channel : in out Channel_Type;
      Element :        Element_Type);

end Muchannel.Writer;
