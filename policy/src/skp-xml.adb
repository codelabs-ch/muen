with Ada.Exceptions;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers;
with Schema.Validators;

with Skp.Xml.Util;
with Skp.Xml.Grammar;

package body Skp.Xml
is

   package DR renames Schema.Dom_Readers;
   package SV renames Schema.Validators;

   -------------------------------------------------------------------------

   procedure Finalize (Object : in out XML_Data_Type)
   is
   begin
      DOM.Core.Nodes.Free (N => Object.Doc);
   end Finalize;

   -------------------------------------------------------------------------

   procedure Parse
     (Data   : in out XML_Data_Type;
      File   :        String;
      Schema :        String)
   is
      Reader     : DR.Tree_Reader;
      File_Input : Input_Sources.File.File_Input;
   begin
      Reader.Set_Grammar (Grammar => Grammar.Get_Grammar (File => Schema));
      Reader.Set_Feature (Name  => Sax.Readers.Schema_Validation_Feature,
                          Value => True);

      begin
         Input_Sources.File.Open (Filename => File,
                                  Input    => File_Input);

         begin
            Reader.Parse (Input => File_Input);

         exception
            when others =>
               Input_Sources.File.Close (Input => File_Input);
               Reader.Free;
               raise;
         end;

         Input_Sources.File.Close (Input => File_Input);
         Data.Doc := Reader.Get_Tree;

      exception
         when SV.XML_Validation_Error =>
            raise Processing_Error with "XML validation error - "
              & Reader.Get_Error_Message;
         when E : others =>
            raise Processing_Error with "Error reading XML file '" & File
              & "' - " & Ada.Exceptions.Exception_Message (X => E);
      end;
   end Parse;

   -------------------------------------------------------------------------

   function To_Policy (Data : XML_Data_Type) return Policy_Type
   is

      package DCD renames DOM.Core.Documents;

      P : Policy_Type;

      --  Add subject specification to policy.
      procedure Add_Subject (Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Subject (Node : DOM.Core.Node)
      is
         use Ada.Strings.Unbounded;

         Name   : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
         Id_Str : constant String  := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "id");
         Id     : constant Natural := Natural'Value (Id_Str);

         Mem_Layout : Memory_Layout_Type;
         Ports      : IO_Ports_Type;

         --  Add memory region to memory layout.
         procedure Add_Mem_Region (Node : DOM.Core.Node);

         --  Add I/O port range to subject I/O ports.
         procedure Add_Port_Range (Node : DOM.Core.Node);

         --  Convert given hex string to word64.
         function To_Word64 (Hex : String) return SK.Word64;

         -------------------------------------------------------------------

         procedure Add_Mem_Region (Node : DOM.Core.Node)
         is
            R      : Memory_Region_Type;
            PM     : SK.Word64;
            VM     : SK.Word64;
            VM_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "virtual_address");
         begin
            PM := To_Word64
              (Hex => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "physical_address"));

            if VM_Str'Length = 0 then
               VM := PM;
            else
               VM := To_Word64 (Hex => VM_Str);
            end if;

            R.Physical_Address := PM;
            R.Virtual_Address  := VM;
            R.Size             := Util.To_Memory_Size
              (Str => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "size"));
            R.Writable         := Boolean'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "writable"));
            R.Executable       := Boolean'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "executable"));

            Mem_Layout.Regions.Append (New_Item => R);
         end Add_Mem_Region;

         -------------------------------------------------------------------

         procedure Add_Port_Range (Node : DOM.Core.Node)
         is
            R            : IO_Port_Range;
            End_Port_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "end");
         begin
            R.Start_Port := SK.Word16 (To_Word64
              (Hex => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "start")));

            if End_Port_Str'Length = 0 then
               R.End_Port := R.Start_Port;
            else
               R.End_Port := SK.Word16 (To_Word64 (Hex => End_Port_Str));
            end if;

            Ports.Ranges.Append (New_Item => R);
         end Add_Port_Range;

         -------------------------------------------------------------------

         function To_Word64 (Hex : String) return SK.Word64
         is
         begin
            return SK.Word64'Value ("16#" & Hex & "#");
         end To_Word64;
      begin
         Mem_Layout.Pml4_Address := To_Word64
           (Hex => Util.Get_Element_Attr_By_Tag_Name
              (Node      => Node,
               Tag_Name  => "memory_layout",
               Attr_Name => "pml4_address"));
         Util.For_Each_Node (Node     => Node,
                             Tag_Name => "memory_region",
                             Process  => Add_Mem_Region'Access);
         Ports.IO_Bitmap_Address := To_Word64
           (Hex => Util.Get_Element_Attr_By_Tag_Name
              (Node      => Node,
               Tag_Name  => "io_ports",
               Attr_Name => "bitmap_address"));
         Util.For_Each_Node (Node     => Node,
                             Tag_Name => "port_range",
                             Process  => Add_Port_Range'Access);

         P.Subjects.Insert
           (New_Item =>
              (Id            => Id,
               Name          => To_Unbounded_String (Name),
               Memory_Layout => Mem_Layout,
               IO_Ports      => Ports));
      end Add_Subject;
   begin
      Util.For_Each_Node (Node     => DCD.Get_Element (Doc => Data.Doc),
                          Tag_Name => "subject",
                          Process  => Add_Subject'Access);
      return P;
   end To_Policy;

end Skp.Xml;
