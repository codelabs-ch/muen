package Skp.Paging.EPT
is

   --  Implementation of EPT paging structures, as specified by Intel SDM
   --  Vol. 3C, section 28.2.

   --  Create a new EPT PML4 entry with specified attributes, referencing a
   --  PDPT located at the given physical address.
   function Create_PML4_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PML4_Entry_Type;

   --  Create a new EPT PDPT entry with specified attributes, referencing a PD
   --  located at the given physical address.
   function Create_PDPT_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PDPT_Entry_Type;

   --  Create a new EPT PD entry with specified attributes, referencing a PT
   --  located at the given physical address.
   function Create_PD_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PD_Entry_Type;

end Skp.Paging.EPT;
