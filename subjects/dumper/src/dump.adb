package body Dump
is

   -------------------------------------------------------------------------

   function Get_Vector (Intr_Info : SK.Word64) return SK.Byte
   is
   begin
      return SK.Byte (Intr_Info);
   end Get_Vector;

   -------------------------------------------------------------------------

   function Is_Valid (Info : SK.Word64) return Boolean
   is
   begin
      return SK.Bit_Test (Value => Info,
                          Pos   => 31);
   end Is_Valid;

end Dump;
