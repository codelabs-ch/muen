
with DOM.Readers;
with GNAT.OS_Lib;
with Sax.Exceptions;

package Testxml_Support is

   type My_Tree_Reader is new DOM.Readers.Tree_Reader with record
      Had_Error : Boolean := False;
      --  Whether any recoverable error was seen

      Error_Msg : GNAT.OS_Lib.String_Access;
      --  The whole cumulative error messages
   end record;
   procedure Error
     (Handler : in out My_Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Override Error so as to make sure recoverable errors are reported as
   --  such. It raises XML_Error

end Testxml_Support;
