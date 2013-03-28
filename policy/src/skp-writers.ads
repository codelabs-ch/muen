package Skp.Writers
is

   --  Write kernel-related policy files to directory specified by name.
   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write subjects-related policy files to directory specified by name.
   procedure Write_Subjects
     (Dir_Name : String;
      Subjects : Subjects_Package.Set);

   IO_Error : exception;

end Skp.Writers;
