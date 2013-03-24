with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Core;           use DOM.Core;
with Schema.Dom_Readers; use Schema.Dom_Readers;

procedure DomSchemaExample is
   Input  : File_Input;
   Reader : Schema.Dom_Readers.Tree_Reader;
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
end DomSchemaExample;
