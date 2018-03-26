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

with Musinfo.Instance;

package Loader.Process_Target
is
   use type Musinfo.Resource_Kind;

   --  Process given target sinfo memory resource.
   procedure Process
     (Sinfo_Mem :     Musinfo.Resource_Type;
      Success   : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid and Sinfo_Mem.Kind = Musinfo.Res_Memory;

end Loader.Process_Target;
