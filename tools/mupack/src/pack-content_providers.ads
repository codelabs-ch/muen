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

with Ada.Streams;
with Ada.Strings.Unbounded;

with DOM.Core;

with Mutools.Image;
with Mutools.Processors;

with Pack.Manifest;

package Pack.Content_Providers
is

   type Param_Type
     (End_Address : Ada.Streams.Stream_Element_Offset;
      Dry_Run     : Boolean)
   is record
      XML_Doc  : DOM.Core.Document;
      Image    : Mutools.Image.Image_Type (End_Address => End_Address);
      Manifest : Pack.Manifest.Manifest_Type;
   end record;

   --  Add file content to system image.
   procedure Process_Files (Data : in out Param_Type);

   --  Add fill content to system image.
   procedure Process_Fills (Data : in out Param_Type);

   --  Register all content providers.
   procedure Register_All;

   --  Set input directory.
   procedure Set_Input_Directory (Dir : String);

   --  Run registered content providers.
   procedure Run (Data : in out Param_Type);

   --  Return number of registered content providers.
   function Get_Count return Natural;

   --  Clear registered providers;
   procedure Clear;

private

   Input_Dir : Ada.Strings.Unbounded.Unbounded_String;

   package Content_Procs is new Mutools.Processors (Param_Type => Param_Type);

end Pack.Content_Providers;
