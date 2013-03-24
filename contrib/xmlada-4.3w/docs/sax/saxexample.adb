with Unicode.CES;    use Unicode.CES;
with Sax.Attributes; use Sax.Attributes;
with Ada.Text_IO;    use Ada.Text_IO;

package body SaxExample is

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
   begin
      Handler.Current_Pref  := Null_Unbounded_String;
      Handler.Current_Value := Null_Unbounded_String;

      if Local_Name = "pref" then
         Handler.Current_Pref :=
           To_Unbounded_String (Get_Value (Atts, "name"));
      end if;    
   end Start_Element;

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Current_Pref /= Null_Unbounded_String then
         Handler.Current_Value := Handler.Current_Value & Ch;
      end if;
   end Characters;

   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
   begin
      if Local_Name = "pref" then
         Put_Line ("Value for """ & To_String (Handler.Current_Pref) 
                   & """ is " & To_String (Handler.Current_Value));
      end if;
   end End_Element;

end SaxExample;
