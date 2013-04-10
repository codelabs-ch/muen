package Skp.Writers
is

   --  Write kernel-related policy files to directory specified by name.
   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write subjects-related policy files to directory specified by name.
   procedure Write_Subjects
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write binary information files to directory specified by name.
   procedure Write_Binaries
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write system-related policy files to directory specified by name.
   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write hardware-related policy files to directory specified by name.
   procedure Write_Hardware
     (Dir_Name : String;
      Policy   : Policy_Type);

   IO_Error : exception;

end Skp.Writers;
