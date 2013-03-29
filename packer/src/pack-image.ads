with SK;

package Pack.Image
is

   --  Add file to kernel image. The file is added as new section with
   --  the specified physical address. The section name is derived from the
   --  filename.
   procedure Add_Section
     (Image    : String;
      Filename : String;
      Address  : SK.Word64);

end Pack.Image;
