with Ada.Directories;

with SK.Utils;

with Pack.OS;

package body Pack.Image
is

   Kernel  : constant String := "obj/kernel";
   Objcopy : constant String := "/usr/bin/objcopy";

   -------------------------------------------------------------------------

   procedure Add_Section
     (Filename : String;
      Address  : SK.Word64)
   is
      Name : constant String := Ada.Directories.Base_Name (Name => Filename);
   begin
      OS.Execute
        (Command => Objcopy & " --add-section ." & Name & "=" & Filename & " "
         & Kernel & " --change-section-address ." & Name & "=0x"
         & SK.Utils.To_Hex (Item => Address) & " --set-section-flags ."
         & Name & "=alloc 2>/dev/null");
   end Add_Section;

end Pack.Image;
