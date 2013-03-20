with Ada.Text_IO;
with Ada.Directories;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Symbols;

with SK.Utils;

package body Skc.Subjects
is

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

   function Read (Binary : String) return SK.Config.Subject_Binary_Type
   is
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
      Bin     : SK.Config.Subject_Binary_Type;
   begin
      begin
         Bfd.Files.Open (File   => File,
                         Name   => Binary);

      exception
         when others =>
            raise Open_Error with "Unable to open binary '" & Binary & "'";
      end;

      if not Bfd.Files.Check_Format
        (File   => File,
         Expect => Bfd.Files.OBJECT)
      then
         raise Binary_Error with "Binary '" & Binary & "' has invalid format";
      end if;

      Bfd.Symbols.Read_Symbols (File    => File,
                                Symbols => Symbols);

      Bin.Entry_Point := Check_Symbol
        (Syms => Symbols,
         Name => "main");
      Bin.Stack_Address := Check_Symbol
        (Syms => Symbols,
         Name => "stack");
      return Bin;
   end Read;

   -------------------------------------------------------------------------

   procedure Write
     (Spec  : String;
      Subjs : Binary_Array)
   is
      use Ada.Text_IO;

      File : File_Type;
   begin
      if Ada.Directories.Exists (Name => Spec) then
         Open (File => File,
               Mode => Out_File,
               Name => Spec);
      else
         Create (File => File,
                 Mode => Out_File,
                 Name => Spec);
      end if;

      Put_Line (File => File,
                Item => "with SK.Config; use SK.Config;");
      Put_Line (File => File,
                Item => "package Skc.Subjects is");
      Put_Line (File => File,
                Item => "   Binaries : constant array (1 .."
                & Subjs'Length'Img & ") of Subject_Binary_Type := (");

      for S in Subjs'Range loop
         Put_Line (File => File,
                   Item => "     (Entry_Point   => 16#"
                   & SK.Utils.To_Hex (Item => Subjs (S).Entry_Point) & "#,");
         Put (File => File,
              Item => "      Stack_Address => 16#"
              & SK.Utils.To_Hex (Item => Subjs (S).Stack_Address) & "#)");
         exit when S = Subjs'Last;

         Put_Line (File => File,
                   Item => ",");
      end loop;

      Put_Line (File => File,
                Item => ");");
      Put_Line (File => File,
                Item => "end Skc.Subjects;");
   end Write;

end Skc.Subjects;
