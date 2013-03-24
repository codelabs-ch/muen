with Sax.Readers;        use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with SaxExample;         use SaxExample;

procedure SaxExample_Main is
   My_Reader : SaxExample.Reader;
   Input     : File_Input;
begin
   Set_Public_Id (Input, "Preferences file");
   Set_System_Id (Input, "pref.xml");
   Open ("pref.xml", Input);

   Set_Feature (My_Reader, Namespace_Prefixes_Feature, False);
   Set_Feature (My_Reader, Namespace_Feature, False);
   Set_Feature (My_Reader, Validation_Feature, False);

   Parse (My_Reader, Input);

   Close (Input);
end SaxExample_Main;
