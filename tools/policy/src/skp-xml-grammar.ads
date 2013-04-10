with Schema.Validators;

package Skp.Xml.Grammar
is

   --  Load grammar from given XML schema file.
   function Get_Grammar (File : String) return Schema.Validators.XML_Grammar;

end Skp.Xml.Grammar;
