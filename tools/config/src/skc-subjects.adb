with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Symbols;

with SK.Utils;

package body Skc.Subjects
is

   --  Check symbol given by name and return its value.
   function Check_Symbol
     (Syms : Bfd.Symbols.Symbol_Table;
      Name : String)
      return SK.Word64;

   --  Open binary file specified by name.
   procedure Open_Bfd_File
     (File : out Bfd.Files.File_Type;
      Name :     String);

   -------------------------------------------------------------------------

   --  Check symbol given by name and return its value.
   function Check_Symbol
     (Syms : Bfd.Symbols.Symbol_Table;
      Name : String)
      return SK.Word64
   is
      use type Bfd.Symbols.Symbol;

      S : constant Bfd.Symbols.Symbol := Bfd.Symbols.Get_Symbol
        (Symbols => Syms,
         Name    => Name);
   begin
      if S = Bfd.Symbols.Null_Symbol then
         raise Binary_Error with "No " & Name & " symbol";
      end if;

      declare
         use type Bfd.Symbol_Flags;

         Sec   : Bfd.Sections.Section;
         Flags : Bfd.Symbol_Flags;
         Value : Bfd.Symbol_Value;
      begin
         Sec   := Bfd.Symbols.Get_Section (S);
         Flags := Bfd.Symbols.Get_Flags (S);
         Value := Bfd.Symbols.Get_Value (S);

         if Bfd.Sections.Is_Undefined_Section (S => Sec) then
            raise Binary_Error with "Undefined symbol " & Name;
         elsif (Flags and Bfd.Symbols.BSF_GLOBAL) = 0 then
            raise Binary_Error with Name & " is not a global symbol";
         end if;

         return SK.Word64 (Value);
      end;
   end Check_Symbol;

   -------------------------------------------------------------------------

   procedure Open_Bfd_File
     (File : out Bfd.Files.File_Type;
      Name :     String)
   is
   begin
      begin
         Bfd.Files.Open (File => File,
                         Name => Name);

      exception
         when others =>
            raise Open_Error with "Unable to open file '" & Name & "'";
      end;

      if not Bfd.Files.Check_Format
        (File   => File,
         Expect => Bfd.Files.OBJECT)
      then
         Bfd.Files.Close (File => File);
         raise Binary_Error with "File '" & Name & "' has invalid format";
      end if;
   end Open_Bfd_File;

   -------------------------------------------------------------------------

   function Read (Binary : String) return Binary_Type
   is
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
      Bin     : Binary_Type;
   begin
      Open_Bfd_File (File => File,
                     Name => Binary);

      begin
         Bfd.Symbols.Read_Symbols (File    => File,
                                   Symbols => Symbols);

         Bin.Entry_Point := Check_Symbol
           (Syms => Symbols,
            Name => "subject_entry");
         Bin.Stack_Address := Check_Symbol
           (Syms => Symbols,
            Name => "stack");
         return Bin;

      exception
         when others =>
            Bfd.Files.Close (File => File);
            raise;
      end;
   end Read;

   -------------------------------------------------------------------------

   procedure Write
     (XML_File : String;
      Subject  : Binary_Type)
   is
      use Ada.Text_IO;

      File : File_Type;
   begin
      if Ada.Directories.Exists (Name => XML_File) then
         Open (File => File,
               Mode => Out_File,
               Name => XML_File);
      else
         Create (File => File,
                 Mode => Out_File,
                 Name => XML_File);
      end if;

      Put (File => File,
           Item => "<initial_state ");

      Put (File => File,
           Item => "stack_address=""" & SK.Utils.To_Hex
             (Item => Subject.Stack_Address) & """");
      Put (File => File,
           Item => " entry_point=""" & SK.Utils.To_Hex
             (Item => Subject.Entry_Point) & """");

      Put_Line (File => File,
                Item => "/>");

      Close (File => File);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Memory_Layout
     (XML_File      : String;
      Binary        : String;
      Start_Address : SK.Word64)
   is
      use Ada.Text_IO;
      use type SK.Word64;
      use type Bfd.Section_Flags;
      use type Bfd.Size_Type;

      Iterator         : Bfd.Sections.Section_Iterator;
      Section          : Bfd.Sections.Section;
      Page_Count       : Natural := 0;
      File             : Bfd.Files.File_Type;
      Mem_File         : File_Type;
      Physical_Address : SK.Word64 := Start_Address;

      --  Write memory layout information about ELF section.
      procedure Write_Section
        (Section          : Bfd.Sections.Section;
         Physical_Address : SK.Word64);

      ----------------------------------------------------------------------

      procedure Write_Section
        (Section          : Bfd.Sections.Section;
         Physical_Address : SK.Word64)
      is
         Code_Section : constant Boolean
           := (Section.Flags and Bfd.Sections.SEC_CODE) /= 0;
         RO_Section   : constant Boolean
           := (Section.Flags and Bfd.Sections.SEC_READONLY) /= 0;
         Section_Size : constant String
           := Ada.Strings.Fixed.Trim (Source => Natural'Image (Page_Count * 4),
                                      Side   => Ada.Strings.Both);
      begin
         Put (File => Mem_File,
              Item => "<memory_region physical_address="""
              & SK.Utils.To_Hex (Item => Physical_Address) & """");
         Put (File => Mem_File,
              Item => " virtual_address="""
              & SK.Utils.To_Hex (Item => SK.Word64 (Section.Vma)) & """");
         Put (File => Mem_File,
              Item => " size=""" & Section_Size & "k""");
         Put (File => Mem_File,
              Item => " alignment=""4k""");
         Put (File => Mem_File,
              Item => " writable=""");
         if RO_Section then
            Put (File => Mem_File,
                 Item => "false"" ");
         else
            Put (File => Mem_File,
                 Item => "true""  ");
         end if;
         Put (File => Mem_File,
              Item => "executable=""");
         if Code_Section then
            Put (File => Mem_File,
                 Item => "true""  ");
         else
            Put (File => Mem_File,
                 Item => "false"" ");
         end if;

         Put_Line (File => Mem_File,
                   Item => "memory_type=""WB""/>");
      end Write_Section;
   begin
      Open_Bfd_File (File => File,
                     Name => Binary);

      if Ada.Directories.Exists (Name => XML_File) then
         Open (File => Mem_File,
               Mode => Out_File,
               Name => XML_File);
      else
         Create (File => Mem_File,
                 Mode => Out_File,
                 Name => XML_File);
      end if;

      begin
         Iterator := Bfd.Sections.Get_Sections (File => File);
         while Bfd.Sections.Has_Element (Iter => Iterator) loop
            Section := Bfd.Sections.Element (Iter => Iterator);
            if (Section.Flags and Bfd.Sections.SEC_ALLOC) /= 0
              and then Section.Size > 0
            then
               Page_Count := Natural (Section.Size / SK.Page_Size);
               if Section.Size rem SK.Page_Size > 0 then
                  Page_Count := Page_Count + 1;
               end if;
               Write_Section (Section          => Section,
                              Physical_Address => Physical_Address);

               Physical_Address := Physical_Address + SK.Word64 (Page_Count)
                 * SK.Page_Size;
            end if;
            Bfd.Sections.Next (Iter => Iterator);
         end loop;

         Close (File => Mem_File);

      exception
         when others =>
            Bfd.Files.Close (File => File);
            Close (File => Mem_File);
      end;
   end Write_Memory_Layout;

end Skc.Subjects;
