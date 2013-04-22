with SK;

package Pack.Image
is

   --  Add file to kernel image. The file is added as new section with
   --  the specified physical address and name.
   procedure Add_Section
     (Image    : String;
      Filename : String;
      Name     : String;
      Address  : SK.Word64);

   --  Convert given source ELF binary to raw binary file.
   procedure To_Binary
     (Src_Elf : String;
      Dst_Bin : String);

end Pack.Image;
