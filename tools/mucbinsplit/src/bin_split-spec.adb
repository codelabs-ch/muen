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

with Muxml.Utils;

with DOM.Core.Documents;
with DOM.Core.Elements;

package body Bin_Split.Spec
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --------------------------------------------------------------------------

   procedure Add_File_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      File_Name       :        String;
      Hash            :        String := "";
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64;
      Reloadable      :        Boolean := False)
   is
      use type Interfaces.Unsigned_64;

      Memory_Node : constant DOM.Core.Element
        := Create_Memory_Node
          (Spec            => Spec,
           Logical         => Logical,
           Writable        => Writable,
           Executable      => Executable,
           Size            => Size,
           Virtual_Address => Virtual_Address);

      File_Node : constant DOM.Core.Element
        := DOM.Core.Documents.Create_Element
          (Doc      => Spec.Doc,
           Tag_Name => "file");
      Hash_Node : DOM.Core.Element;
   begin
      Muxml.Utils.Append_Child
        (Node      => Memory_Node,
         New_Child => File_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "filename",
         Value => File_Name);

      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "offset",
         Value => "none");

      if Hash /= "" then
         Hash_Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "hash");
         Muxml.Utils.Append_Child
           (Node      => Memory_Node,
            New_Child => Hash_Node);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Hash_Node,
            Name  => "value",
            Value => Hash);
      end if;

      if Reloadable and Writable then
         --  Swap out with target region:
         --  * Add "_src" suffix to logical name
         --  * Clear writable flag
         --  * Set different address

         DOM.Core.Elements.Set_Attribute
           (Elem  => Memory_Node,
            Name  => "logical",
            Value => Logical & "_src");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Memory_Node,
            Name  => "writable",
            Value => "false");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Memory_Node,
            Name  => "virtualAddress",
            Value => Mutools.Utils.To_Hex
              (Number => Virtual_Address or 16#ffff_8000_0000_0000#));
         declare
            Target_Mem_Node : constant DOM.Core.Element
              := Create_Memory_Node
                (Spec            => Spec,
                 Logical         => Logical,
                 Writable        => Writable,
                 Executable      => Executable,
                 Size            => Size,
                 Virtual_Address => Virtual_Address);
         begin
            if Hash /= "" then
               Hash_Node := DOM.Core.Documents.Create_Element
                 (Doc      => Spec.Doc,
                  Tag_Name => "hash");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Hash_Node,
                  Name  => "value",
                  Value => Hash);
               Muxml.Utils.Append_Child
                 (Node      => Target_Mem_Node,
                  New_Child => Hash_Node);
            end if;
         end;
      end if;
   end Add_File_Entry;

   --------------------------------------------------------------------------

   procedure Add_Fill_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Fill_Pattern    :        Interfaces.Unsigned_8 := 0;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64)
   is
      Memory_Node : constant DOM.Core.Element
        := Create_Memory_Node
          (Spec            => Spec,
           Logical         => Logical,
           Writable        => Writable,
           Executable      => Executable,
           Size            => Size,
           Virtual_Address => Virtual_Address);

      Fill_Node : constant DOM.Core.Element
        := DOM.Core.Documents.Create_Element
          (Doc      => Spec.Doc,
           Tag_Name => "fill");
   begin
      Muxml.Utils.Append_Child
        (Node      => Memory_Node,
         New_Child => Fill_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Fill_Node,
         Name  => "pattern",
         Value => Mutools.Utils.To_Hex
           (Number     => Interfaces.Unsigned_64 (Fill_Pattern),
            Byte_Short => True));
   end Add_Fill_Entry;

   --------------------------------------------------------------------------

   function Create_Memory_Node
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64)
      return DOM.Core.Element
   is
      use type DOM.Core.Node;

      Provides_Node : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Spec.Doc,
           XPath => "/component/provides");

      Memory_Node : constant DOM.Core.Element
        := Mutools.XML_Utils.Create_Component_Memory_Node
          (Policy       => Spec,
           Logical_Name => Logical,
           Writable     => Writable,
           Executable   => Executable,
           Size         => Mutools.Utils.To_Hex (Number => Size),
           Address      => Mutools.Utils.To_Hex (Number => Virtual_Address));
   begin
      if Provides_Node = null then
         Provides_Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "provides");

         Muxml.Utils.Append_Child
           (Node      => DOM.Core.Documents.Get_Element (Doc => Spec.Doc),
            New_Child => Provides_Node);
      end if;

      Muxml.Utils.Append_Child
        (Node      => Provides_Node,
         New_Child => Memory_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Memory_Node,
         Name  => "type",
         Value => "subject_binary");

      return Memory_Node;
   end Create_Memory_Node;

   -------------------------------------------------------------------------

   procedure Set_RIP
     (Spec        : in out Muxml.XML_Data_Type;
      Entry_Point :        Interfaces.Unsigned_64)
   is
      use type DOM.Core.Node;

      Parent_Node : DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Spec.Doc,
         XPath => "/component/requires");
      Node : DOM.Core.Node;
   begin
      if Parent_Node = null then
         Parent_Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "requires");
         Muxml.Utils.Insert_Before
           (Parent    => DOM.Core.Documents.Get_Element (Doc => Spec.Doc),
            New_Child => Parent_Node,
            Ref_Child => "provides");
      end if;

      Node := Muxml.Utils.Get_Element
        (Doc   => Parent_Node,
         XPath => "vcpu");
      if Node = null then
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "vcpu");
         Muxml.Utils.Insert_Before
           (Parent    => Parent_Node,
            New_Child => Node,
            Ref_Names => (U ("memory"),
                          U ("channels"),
                          U ("devices"),
                          U ("events")));
      end if;

      Parent_Node := Node;
      Node := Muxml.Utils.Get_Element
        (Doc   => Parent_Node,
         XPath => "registers");
      if Node = null then
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "registers");
         Muxml.Utils.Append_Child (Node      => Parent_Node,
                                   New_Child => Node);
      end if;

      Parent_Node := Node;
      Node := Muxml.Utils.Get_Element
        (Doc   => Parent_Node,
         XPath => "gpr");
      if Node = null then
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "gpr");
         Muxml.Utils.Insert_Before
           (Parent    => Parent_Node,
            New_Child => Node,
            Ref_Names => (U ("cr0"),
                          U ("cr4"),
                          U ("segments")));
      end if;
      Parent_Node := Node;
      Node := Muxml.Utils.Get_Element
        (Doc   => Parent_Node,
         XPath => "rip");
      if Node = null then
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name => "rip");
         Muxml.Utils.Insert_Before
           (Parent    => Parent_Node,
            New_Child => Node,
            Ref_Names => (U ("rsp"),
                          U ("rax"),
                          U ("rbx"),
                          U ("rcx"),
                          U ("rdx"),
                          U ("rdi"),
                          U ("rsi"),
                          U ("rbp"),
                          U ("r08"),
                          U ("r09"),
                          U ("r10"),
                          U ("r11"),
                          U ("r12"),
                          U ("r13"),
                          U ("r14"),
                          U ("r15")));
      end if;
      Muxml.Utils.Set_Element_Value
        (Doc   => Node,
         XPath => ".",
         Value => Mutools.Utils.To_Hex (Number => Entry_Point));
   end Set_RIP;

end Bin_Split.Spec;
