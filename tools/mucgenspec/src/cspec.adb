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

with Muxml;
with Mulog;
with Mutools.Utils;
with Mutools.Templates;

with Cspec.Utils;
with Cspec.Generators;

with String_Templates;

package body Cspec
is

   --  Create template content for component with given name.
   function Create_Template
     (Comp_Name : String;
      Content   : String)
      return Mutools.Templates.Template_Type;

   --  Replace the pattern in the template with the specified content and write
   --  the result to the file given by name. Do nothing if the content string
   --  is empty.
   procedure Create_Child_Package
     (Tmpl     : in out Mutools.Templates.Template_Type;
      Pattern  :        String;
      Content  :        String;
      Filename :        String);

   -------------------------------------------------------------------------

   procedure Create_Child_Package
     (Tmpl     : in out Mutools.Templates.Template_Type;
      Pattern  :        String;
      Content  :        String;
      Filename :        String)
   is
   begin
      if Content'Length = 0 then
         return;
      end if;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => Pattern,
         Content  => Content);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Create_Child_Package;

   -------------------------------------------------------------------------

   function Create_Template
     (Comp_Name : String;
      Content   : String)
      return Mutools.Templates.Template_Type
   is
   begin
      return T : Mutools.Templates.Template_Type do
         T := Mutools.Templates.Create (Content => Content);
         Mutools.Templates.Replace
           (Template => T,
            Pattern  => "__component_name__",
            Content  => Mutools.Utils.To_Ada_Identifier (Str => Comp_Name));
      end return;
   end Create_Template;

   -------------------------------------------------------------------------

   procedure Run
     (Input_Spec       : String;
      Output_Directory : String;
      Package_Name     : String := "")
   is
      Spec : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing component specification '"
                 & Input_Spec & "'");

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => Input_Spec);

      declare
         Tmpl : Mutools.Templates.Template_Type;
         Component_Name : constant String
           := Utils.Get_Component_Name (Spec => Spec);
         Pack_Name : constant String
           := (if Package_Name'Length > 0
               then Package_Name else Component_Name);
         Pack_Name_Lower : constant String
           := Ada.Characters.Handling.To_Lower (Item => Pack_Name);
         Fname_Base : constant String
           := Output_Directory & "/" & Pack_Name_Lower & "_component";
         Config : constant String
           := Generators.Get_Config_Str (Spec => Spec);
         Memory : constant String
           := Generators.Get_Memory_Str (Spec => Spec);
         Channels : constant String
           := Generators.Get_Channels_Str (Spec => Spec);
         Devices : constant String
           := Generators.Get_Devices_Str (Spec => Spec);
         Events : constant String
           := Generators.Get_Event_Str (Spec => Spec);
         Mem_Arrays : constant String
           := Generators.Get_Memory_Arrays_Str (Spec => Spec);
         Channel_Arrays : constant String
           := Generators.Get_Channel_Arrays_Str (Spec => Spec);
      begin
         if not Ada.Directories.Exists (Name => Output_Directory) then
            Ada.Directories.Create_Path (New_Directory => Output_Directory);
         end if;

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_ads);

         if Config'Length = 0
           and then Memory'Length = 0
           and then Channels'Length = 0
           and then Devices'Length = 0
           and then Events'Length = 0
           and then Mem_Arrays'Length = 0
           and then Channel_Arrays'Length = 0
         then
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__name_types__",
               Content  => "");
            Mutools.Templates.Write
              (Template => Tmpl,
               Filename => Fname_Base & ".ads");
            return;
         end if;

         Mulog.Log (Msg => "Generating resource constants for '"
                    & Component_Name & "' in directory '" & Output_Directory
                    & "'");

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__name_types__",
            Content  => Utils.Get_Name_Types_Str);
         Mutools.Templates.Write
           (Template => Tmpl,
            Filename => Fname_Base & ".ads");
         Mutools.Templates.Write
           (Template => Create_Template
              (Comp_Name => Pack_Name,
               Content   => String_Templates.component_adb),
            Filename => Fname_Base & ".adb");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_config_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__config__",
            Content  => Config,
            Filename => Fname_Base & "-config.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_memory_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__memory__",
            Content  => Memory,
            Filename => Fname_Base & "-memory.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_channels_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__channels__",
            Content  => Channels,
            Filename => Fname_Base & "-channels.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_devices_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__devices__",
            Content  => Devices,
            Filename => Fname_Base & "-devices.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_events_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__events__",
            Content  => Events,
            Filename => Fname_Base & "-events.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_memory_arrays_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__memory_arrays__",
            Content  => Mem_Arrays,
            Filename => Fname_Base & "-memory_arrays.ads");

         Tmpl := Create_Template
           (Comp_Name => Pack_Name,
            Content   => String_Templates.component_channel_arrays_ads);
         Create_Child_Package
           (Tmpl     => Tmpl,
            Pattern  => "__channel_arrays__",
            Content  => Channel_Arrays,
            Filename => Fname_Base & "-channel_arrays.ads");

         Mulog.Log (Msg => "Specs generated successfully");

      end;
   end Run;

end Cspec;
