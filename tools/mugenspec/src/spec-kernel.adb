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

with DOM.Core.Nodes;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;
with Mutools.Match;

with String_Templates;

package body Spec.Kernel
is

   -------------------------------------------------------------------------

   procedure Write_Project_File
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Filename    : constant String := Output_Dir & "/" & "policy.gpr";
      IOMMUs      : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => Policy,
           Left_XPath  => "/system/kernel/devices/device",
           Right_XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference'Access);
      IOMMU_Count : constant Natural := DOM.Core.Nodes.Length
        (List => IOMMUs.Right);
      Tmpl        : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing policy project file to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.policy_gpr);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__feature_iommu__",
         Content  => (if IOMMU_Count > 0 then "enable" else "disable"));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write_Project_File;

end Spec.Kernel;
