with DOM.Readers;      use DOM.Readers;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Sax.Exceptions;   use Sax.Exceptions;

package body Testxml_Support is

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out My_Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      Tmp : String_Access := Handler.Error_Msg;
   begin
      if Tmp = null then
         Handler.Error_Msg := new String'(Get_Message (Except));
      else
         Handler.Error_Msg := new String'
           (Tmp.all & ASCII.LF & Get_Message (Except));
         Free (Tmp);
      end if;
      Handler.Had_Error := True;
   end Error;

end Testxml_Support;
