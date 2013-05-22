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

   --  Create a new EPT PDPT entry with specified attributes The map page
   --  parameter specifies if the entry maps a 1 GB page or references a PD
   --  located at the given physical address.
   function Create_PDPT_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Memory_Type_Type)
      return PDPT_Entry_Type;

   --  Create a new EPT PD entry with specified attributes. The map page
   --  parameter specifies if the entry maps a 2 MB page or references a PT
   --  located at the given physical address.
   function Create_PD_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Memory_Type_Type)
      return PD_Entry_Type;

   --  Create a new PT entry with specified attributes, mapping a 4 KB page of
   --  memory at the given physical address.
   function Create_PT_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Memory_Type_Type)
      return PT_Entry_Type;

end Skp.Paging.EPT;
