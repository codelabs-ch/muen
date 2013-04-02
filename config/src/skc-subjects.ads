with SK;

with Skp;

package Skc.Subjects
is

   --  Information about a subject binary.
   type Binary_Type is record
      Entry_Point   : SK.Word64;
      Stack_Address : SK.Word64;
   end record;

   type Binary_Array is array (Skp.Subject_Id_Type) of Binary_Type;

   --  Read and analyze subject binary. Raises Binary_Error if a check fails.
   function Read (Binary : String) return Binary_Type;

   --  Write subject binaries specification to given file.
   procedure Write
     (Spec  : String;
      Subjs : Binary_Array);

   Open_Error   : exception;
   Binary_Error : exception;

end Skc.Subjects;
