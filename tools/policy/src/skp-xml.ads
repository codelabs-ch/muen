private with DOM.Core;
private with Ada.Finalization;

package Skp.Xml
is

   --  DOM tree of XML document.
   type XML_Data_Type is private;

   --  Parse the contents of given file into the DOM data structure. The XML
   --  data is validated against the given XML schema.
   procedure Parse
     (Data   : in out XML_Data_Type;
      File   :        String;
      Schema :        String);

   --  Create SK system policy from given XML document.
   function To_Policy (Data : XML_Data_Type) return Policy_Type;

   Processing_Error : exception;

private

   type XML_Data_Type is new Ada.Finalization.Controlled with record
      Doc : DOM.Core.Document;
   end record;

   --  Free XML document.
   overriding
   procedure Finalize (Object : in out XML_Data_Type);

end Skp.Xml;
