package Skp.Writers
is

   --  Write subject specs to package with given name. The output is stored to
   --  the file specified by name.
   procedure Write_Subjects
     (File_Name    : String;
      Package_Name : String;
      Policy       : Policy_Type);

   --  Write subject pagetables to directory specified by name. The generated
   --  pagetable files are named as follows: Dir_Name/SUBJECT_NAME.pt.
   procedure Write_Pagetables
     (Dir_Name : String;
      Policy   : Policy_Type);

end Skp.Writers;
