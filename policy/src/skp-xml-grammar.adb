with Ada.Exceptions;

with Input_Sources.File;
with Schema.Schema_Readers;

package body Skp.Xml.Grammar
is

   Current_Grammar : Schema.Validators.XML_Grammar
     := Schema.Validators.No_Grammar;

   -------------------------------------------------------------------------

   function Get_Grammar (File : String) return Schema.Validators.XML_Grammar
   is
      use type Schema.Validators.XML_Grammar;

      Reader     : Schema.Schema_Readers.Schema_Reader;
      File_Input : Input_Sources.File.File_Input;
   begin
      if Current_Grammar = Schema.Validators.No_Grammar then
         Input_Sources.File.Open (Filename => File,
                                  Input    => File_Input);

         begin
            Schema.Schema_Readers.Parse
              (Parser => Reader,
               Input  => File_Input);

         exception
            when others =>
               Input_Sources.File.Close (Input => File_Input);
               Reader.Free;
               raise;
         end;

         Input_Sources.File.Close (Input => File_Input);
         Current_Grammar := Reader.Get_Grammar;
      end if;

      return Current_Grammar;

   exception
      when Schema.Validators.XML_Validation_Error =>
         raise Processing_Error with "XML validation error - "
           & Reader.Get_Error_Message;
      when E : others =>
         raise Processing_Error with "Error reading XSD file '" & File
           & "' - " & Ada.Exceptions.Exception_Message (X => E);
   end Get_Grammar;

end Skp.Xml.Grammar;
