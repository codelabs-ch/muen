private with Ada.Strings.Unbounded;

package Skp.Templates
is

   type Template_Type is private;

   --  Load template with given filename. By default templates are loaded from
   --  the template store (i.e. filenames are relative to the current template
   --  directory).
   function Load
     (Filename  : String;
      Use_Store : Boolean := True)
      return Template_Type;

   --  Returns the current size of the template buffer.
   function Get_Size (Template : Template_Type) return Natural;

   --  Set template store directory, defaults to './templates'
   procedure Set_Template_Dir (Path : String);

   --  Replace pattern occurrence in template with given content. Only the
   --  first occurrence of the given pattern is replaced.
   procedure Replace
     (Template : in out Template_Type;
      Pattern  :        String;
      Content  :        String);

   --  Write (processed) template to given file.
   procedure Write
     (Template : Template_Type;
      Filename : String);

   Pattern_Not_Found : exception;

private

   type Template_Type is record
      Data : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Skp.Templates;
