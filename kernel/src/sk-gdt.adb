package body SK.GDT
is

   --  Global descriptor table pointer
   GDT_Pointer : Descriptors.Pseudo_Descriptor_Type;
   pragma Import (C, GDT_Pointer, "gdt_ptr");
   --# assert GDT_Pointer'Always_Valid;

   -------------------------------------------------------------------------

   function Get_GDT_Pointer return Descriptors.Pseudo_Descriptor_Type
   is
   begin
      return GDT_Pointer;
   end Get_GDT_Pointer;

end SK.GDT;
