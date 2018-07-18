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

package Cspec
is

   --  Start spec generation for given input component specification. XIncludes
   --  are resolved using the specified include path.
   --
   --  If the output spec path is not nil, the processed XML data is written to
   --  the given path.
   --
   --  If the package name is not nil, it will be used in the generated Ada
   --  code instead of the component name declared in the input component
   --  specification.
   procedure Run
     (Input_Spec       : String;
      Output_Spec      : String := "";
      Output_Directory : String;
      Include_Path     : String;
      Package_Name     : String := "");

   Component_Not_Found : exception;

end Cspec;
