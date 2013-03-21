package Test_Utils
is

   --  Compare two files byte-wise. Returns True if both files are equal.
   --  The two files are closed but not removed after comparison. Raises
   --  Open_File_Error exception if one of the given files cannot be opened.
   function Equal_Files
     (Filename1 : String;
      Filename2 : String)
      return Boolean;

   Open_File_Error : exception;

end Test_Utils;
