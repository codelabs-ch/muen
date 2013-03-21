package SK.Config
is

   --  Information about a subject binary.
   type Subject_Binary_Type is record
      Entry_Point   : SK.Word64;
      Stack_Address : SK.Word64;
   end record;

end SK.Config;
