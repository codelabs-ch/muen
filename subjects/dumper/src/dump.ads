with SK;

package Dump
is

   --  Returns True if the interruption info is valid.
   function Is_Valid (Info : SK.Word64) return Boolean;

   --  Returns the Vector of the interrupt or exception.
   function Get_Vector (Intr_Info : SK.Word64) return SK.Byte;

end Dump;
