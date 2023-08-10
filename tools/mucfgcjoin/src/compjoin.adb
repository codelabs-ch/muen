--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Documents;

with Mulog;

with Muxml;
with Muxml.Utils;
with Muxml.Grammar;
with Muxml.Grammar_Tools;

with Mutools.Strings;

with Compjoin.Utils;

package body Compjoin
is

   --  Add components section if it is missing in the given system policy.
   procedure Add_Components_Section (Policy : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Add_Components_Section (Policy : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Root           : constant DOM.Core.Node
        := DOM.Core.Documents.Get_Element (Policy.Doc);
      Component_Node : DOM.Core.Node
        := Muxml.Utils.Get_Unique_Element_Child (Parent     => Root,
                                                 Child_Name => "components");
   begin
      if Component_Node /= null then
         return;
      end if;

      Component_Node := DOM.Core.Documents.Create_Element
                (Doc      => Policy.Doc,
                 Tag_Name => "components");
      Muxml.Utils.Insert_Child
           (Parent    => Root,
            New_Child => Component_Node);
   end Add_Components_Section;

   -------------------------------------------------------------------------

   procedure Run
     (Input_File     : String;
      Output_File    : String;
      Component_List : String)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing system policy '" & Input_File & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src_Ext,
                   File => Input_File);
      --  Load schema information for schema-dependent insertion of nodes.
      Muxml.Grammar_Tools.Init_Order_Information
        (Schema_XML_Data => Muxml.Grammar.Schema_Map
           (Muxml.Format_Src_Ext).XSD.all);

      Add_Components_Section (Policy => Policy);

      declare
         Component_Files : constant Mutools.Strings.String_Array
           := Mutools.Strings.Tokenize (Str       => Component_List,
                                        Separator => ',');
      begin
         for CSpec of Component_Files loop
            declare
               Spec_File : constant String
                 := Ada.Strings.Unbounded.To_String (CSpec);
            begin
               Mulog.Log (Msg => "Joining component XML specification '"
                          & Spec_File & "'");
               Utils.Add_Component
                 (Policy         => Policy,
                  Component_File => Spec_File);
            end;
         end loop;
      end;

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src_Ext,
                   File => Output_File);
      Mulog.Log (Msg => "Successfully wrote joined system policy to '"
                 & Output_File & "'");
   end Run;

end Compjoin;
