--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Mutools.Utils;
with Mutools.XML_Utils;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

package body Bin_Split.Spec
is

   --------------------------------------------------------------------------

   procedure Add_File_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      File_Name       :        String;
      Hash            :        String := "";
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64)
   is
      Child, Grand_Child : DOM.Core.Element;
   begin
      Child := Mutools.XML_Utils.Create_Component_Memory_Node
        (Policy       => Spec,
         Logical_Name => Logical,
         Writable     => Writable,
         Executable   => Executable,
         Address      => Mutools.Utils.To_Hex (Number => Virtual_Address),
         Size         => Mutools.Utils.To_Hex (Number => Size));

      Child := DOM.Core.Nodes.Append_Child
        (N         => DOM.Core.Documents.Get_Element (Spec.Doc),
         New_Child => Child);

      Grand_Child := DOM.Core.Documents.Create_Element
        (Doc => Spec.Doc,
         Tag_Name => "file");

      Grand_Child := DOM.Core.Nodes.Append_Child
        (N         => Child,
         New_Child => Grand_Child);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Grand_Child,
         Name  => "filename",
         Value => File_Name);

      if Hash /= "" then
         Grand_Child := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "hash");

         Grand_Child := DOM.Core.Nodes.Append_Child
           (N         => Child,
            New_Child => Grand_Child);

         DOM.Core.Elements.Set_Attribute
           (Elem  => Grand_Child,
            Name  => "value",
            Value => Hash);
      end if;
   end Add_File_Entry;

   --------------------------------------------------------------------------

   procedure Add_Fill_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Fill_Pattern    :        Interfaces.Unsigned_64 := 0;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64)
   is
      Child, Grand_Child : DOM.Core.Element;
   begin
      Child := Mutools.XML_Utils.Create_Component_Memory_Node
        (Policy       => Spec,
         Logical_Name => Logical,
         Writable     => Writable,
         Executable   => Executable,
         Address      => Mutools.Utils.To_Hex (Number => Virtual_Address),
         Size         => Mutools.Utils.To_Hex (Number => Size));

      Child := DOM.Core.Nodes.Append_Child
        (N         => DOM.Core.Documents.Get_Element (Spec.Doc),
         New_Child => Child);

      Grand_Child := DOM.Core.Documents.Create_Element
        (Doc      => Spec.Doc,
         Tag_Name =>"fill");

      Grand_Child := DOM.Core.Nodes.Append_Child
        (N         => Child,
         New_Child => Grand_Child);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Grand_Child,
         Name  => "pattern",
         Value => Mutools.Utils.To_Hex (Number => Fill_Pattern));
   end Add_Fill_Entry;

end Bin_Split.Spec;
