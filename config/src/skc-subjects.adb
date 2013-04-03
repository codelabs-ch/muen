with Ada.Text_IO;
with Ada.Directories;

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

   function Read (Binary : String) return Binary_Type
   is
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
      Bin     : Binary_Type;
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
         raise Binary_Error with "File '" & Binary & "' has invalid format";
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

end Skc.Subjects;
