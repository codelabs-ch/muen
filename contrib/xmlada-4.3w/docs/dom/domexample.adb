with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;

procedure DomExample is
   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;
begin
   Set_Public_Id (Input, "Preferences file");
   Open ("pref.xml", Input);

   Set_Feature (Reader, Validation_Feature, False);
   Set_Feature (Reader, Namespace_Feature, False);

   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader); 

   Free (Reader);
end DomExample;
