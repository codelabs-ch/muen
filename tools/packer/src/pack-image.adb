with SK.Utils;

with Pack.OS;

package body Pack.Image
is

   Objcopy : constant String := "/usr/bin/objcopy";

   -------------------------------------------------------------------------

   procedure Add_Section
     (Image    : String;
      Filename : String;
      Name     : String;
      Address  : SK.Word64)
   is
   begin
      OS.Execute
        (Command => Objcopy & " --add-section ." & Name & "=" & Filename & " "
         & Image & " --change-section-address ." & Name & "=0x"
         & SK.Utils.To_Hex (Item => Address) & " --set-section-flags ."
         & Name & "=alloc 2>/dev/null");
   end Add_Section;

   -------------------------------------------------------------------------

   procedure To_Binary
     (Src_Elf : String;
      Dst_Bin : String)
   is
   begin
      OS.Execute (Command => Objcopy & " -O binary --set-section-flags "
                  & ".bss=alloc,load,contents " & Src_Elf & " " & Dst_Bin);
   end To_Binary;

end Pack.Image;
