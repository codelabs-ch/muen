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

with Mutools.Immutable_Processors;

with Pack.Content_Providers;

package Pack.Post_Checks
is

   --  Check Multiboot header presence.
   procedure Multiboot_Header (Data : Content_Providers.Param_Type);

   --  Register all post-checks.
   procedure Register_All;

   --  Run registered post-checks.
   procedure Run (Data : Content_Providers.Param_Type);

   --  Return number of registered post-checks.
   function Get_Count return Natural;

   --  Clear registered post checks.
   procedure Clear;

   Check_Error : exception;

private

   package Check_Procs is new
     Mutools.Immutable_Processors (Param_Type => Content_Providers.Param_Type);

end Pack.Post_Checks;
