package Skp.Specs
is

   --  Write subject specs to package with given name. The output is stored to
   --  the file specified by name.
   procedure Write_Subjects
     (File_Name    : String;
      Package_Name : String;
      Policy       : Policy_Type);

end Skp.Specs;
