with Sax.Readers;        use Sax.Readers;
with Schema.Readers;     use Schema.Readers;
with Input_Sources.File; use Input_Sources.File;

procedure SchemaExample is
   Input : File_Input;
   My_Reader : Validating_Reader;
begin
   Set_Public_Id (Input, "Preferences file");
   Open ("pref.xml", Input);

   Set_Feature (My_Reader, Schema_Validation_Feature, True);
   Parse (My_Reader, Input);

   Close (Input);
end SchemaExample;
