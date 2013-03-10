with SK;

package Config_Subjects
is

   --  Information about a subject binary.
   type Binary_Type is record
      Entry_Point   : SK.Word64;
      Stack_Address : SK.Word64;
   end record;

end Config_Subjects;
