--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Elements;
with DOM.Core.Documents;

with Muxml.Utils;

package body Expanders.Features
is

   -------------------------------------------------------------------------

   procedure Add_Default_Features (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      System : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system");
      Features : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => System,
           XPath => "features");

      --  Create top-level features node if it does not exist.
      procedure Create_Features_Node;

      --  Create default feature with given name.
      procedure Create_Default
        (Name    : String;
         Enabled : Boolean);

      ----------------------------------------------------------------------

      procedure Create_Default
        (Name    : String;
         Enabled : Boolean)
      is
         F : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Features,
              XPath => Name);
         Status : constant String := (if Enabled then "true" else "false");
      begin
         if F = null then
            F := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => Name);
            DOM.Core.Elements.Set_Attribute
              (Elem  => F,
               Name  => "enabled",
               Value => Status);
            Muxml.Utils.Append_Child
              (Node      => Features,
               New_Child => F);
         end if;
      end Create_Default;

      ----------------------------------------------------------------------

      procedure Create_Features_Node
      is
      begin
         Features := DOM.Core.Nodes.Insert_Before
           (N         => System,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "features"),
            Ref_Child => Muxml.Utils.Get_Element
              (Doc   => System,
               XPath => "platform"));
      end Create_Features_Node;
   begin
      if Features = null then
         Create_Features_Node;
      end if;

      Create_Default (Name    => "iommu",
                      Enabled => True);
      Create_Default (Name    => "xsave",
                      Enabled => True);
   end Add_Default_Features;

end Expanders.Features;
