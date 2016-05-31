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

with Ada.Directories;
with Ada.Characters.Handling;

with Mulog;
with Muxml;
with Mutools.Utils;
with Mutools.Templates;

with Cspec.Utils;
with Cspec.Generators;

with String_Templates;

package body Cspec
is

   --  Initialize template content for component with given name.
   procedure Create
     (T         : out Mutools.Templates.Template_Type;
      Comp_Name :     String;
      Content   :     String);

   -------------------------------------------------------------------------

   procedure Create
     (T         : out Mutools.Templates.Template_Type;
      Comp_Name :     String;
      Content   :     String)
   is
   begin
      T := Mutools.Templates.Create (Content => Content);
      Mutools.Templates.Replace
        (Template => T,
         Pattern  => "__component_name__",
         Content  => Mutools.Utils.To_Ada_Identifier (Str => Comp_Name));
   end Create;

   -------------------------------------------------------------------------

   procedure Run
     (Policy_File      : String;
      Component_Name   : String;
      Output_Directory : String)
   is
      Tmpl   : Mutools.Templates.Template_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Generating '" & Component_Name & "' component specs "
                 & "in '" & Output_Directory & "' directory");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Policy_File);
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      if not Utils.Is_Present
        (Policy    => Policy,
         Comp_Name => Component_Name)
      then
         raise Component_Not_Found with "Component '" & Component_Name
           & "' not found in the policy";
      end if;

      declare
         Comp_Name_Lower : constant String
           := Ada.Characters.Handling.To_Lower (Item => Component_Name);
         Memory : constant String
           := Generators.Get_Memory_Str
             (Policy    => Policy,
              Comp_Name => Component_Name);
         Channels : constant String
           := Generators.Get_Channels_Str
             (Policy    => Policy,
              Comp_Name => Component_Name);
      begin
         if Memory'Length = 0 and then Channels'Length = 0 then
            Mulog.Log (Msg => "No resources found, nothing to do");
            return;
         end if;

         if not Ada.Directories.Exists (Name => Output_Directory) then
            Ada.Directories.Create_Path (New_Directory => Output_Directory);
         end if;

         --  Top-level package.

         Create (T         => Tmpl,
                 Comp_Name => Component_Name,
                 Content   => String_Templates.component_ads);
         Mutools.Templates.Write
           (Template => Tmpl,
            Filename => Output_Directory & "/" & Comp_Name_Lower
            & "_component.ads");

         --  Memory child package.

         if Memory'Length > 0 then
            Create (T         => Tmpl,
                    Comp_Name => Component_Name,
                    Content   => String_Templates.component_memory_ads);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__memory__",
               Content  => Memory);
            Mutools.Templates.Write
              (Template => Tmpl,
               Filename => Output_Directory & "/" & Comp_Name_Lower
               & "_component-memory.ads");
         end if;

         --  Channels child package.

         if Channels'Length > 0 then
            Create (T         => Tmpl,
                    Comp_Name => Component_Name,
                    Content   => String_Templates.component_channels_ads);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__channels__",
               Content  => Channels);
            Mutools.Templates.Write
              (Template => Tmpl,
               Filename => Output_Directory & "/" & Comp_Name_Lower
               & "_component-channels.ads");
         end if;

         Mulog.Log (Msg => "Specs generated successfully");
      end;
   end Run;

end Cspec;
