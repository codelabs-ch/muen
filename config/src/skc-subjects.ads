with Config_Subjects;

package Skc.Subjects
is

   type Binary_Array is array (Positive range <>)
     of Config_Subjects.Binary_Type;

   --  Read and analyze subject binary. Raises Binary_Error if a check fails.
   function Read (Binary : String) return Config_Subjects.Binary_Type;

   --  Write subject binaries specification to given file.
   procedure Write
     (Spec  : String;
      Subjs : Binary_Array);

   Open_Error   : exception;
   Binary_Error : exception;

end Skc.Subjects;
