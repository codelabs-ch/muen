with SK;

package Skc.Subjects
is

   --  Information about a subject binary.
   type Binary_Type is record
      Entry_Point   : SK.Word64;
      Stack_Address : SK.Word64;
   end record;

   --  Read and analyze subject binary. Raises Binary_Error if a check fails.
   function Read (Binary : String) return Binary_Type;

   --  Write subject binary specification to given XML file.
   procedure Write
     (XML_File : String;
      Subject  : Binary_Type);

   Open_Error   : exception;
   Binary_Error : exception;

end Skc.Subjects;
