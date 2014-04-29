--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with bootparam_h;

with DOM.Core;

package Zp.Utils
is

   --  Create e820 map based on given virtual memory regions.
   function Create_e820_Map
     (Memory : DOM.Core.Node_List)
      return bootparam_h.boot_params_e820_map_array;

   Null_e820_Map : constant bootparam_h.boot_params_e820_map_array
     := (others => (addr   => 0,
                    size   => 0,
                    c_type => 0));

end Zp.Utils;
