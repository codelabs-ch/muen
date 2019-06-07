--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with Mutools.System_Config;

package body Merge.Checks
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Required_Config_Values (Policy : Muxml.XML_Data_Type)
   is
      Required_Cfgs : constant array  (1 .. 3) of Unbounded_String
        := (1 => To_Unbounded_String (Source => "system"),
            2 => To_Unbounded_String (Source => "hardware"),
            3 => To_Unbounded_String (Source => "platform"));
   begin
      for Cfg of Required_Cfgs loop
         if not Mutools.System_Config.Has_String
           (Data => Policy,
            Name => To_String (Source => Cfg))
         then
            raise Validation_Error with "Required string config value '"
              & To_String (Source => Cfg) & "' missing";
         end if;
      end loop;
   end Required_Config_Values;

end Merge.Checks;
