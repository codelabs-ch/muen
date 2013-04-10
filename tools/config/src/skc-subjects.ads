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

   --  Write subject memory layout specification for a given binary with
   --  specified physical start address to given XML file.
   procedure Write_Memory_Layout
     (XML_File      : String;
      Binary        : String;
      Start_Address : SK.Word64);

   Open_Error   : exception;
   Binary_Error : exception;

end Skc.Subjects;
