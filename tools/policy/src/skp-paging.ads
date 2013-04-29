with SK;

package Skp.Paging
is

   --  Implementation of IA-32e paging structures, as specified by Intel SDM
   --  Vol. 3A, section 4.5.

   --  All paging structure types (PML4, PDPT, PD, PT) have 512 entries.
   type Table_Range is range 1 .. 512;

   --  Table entry type. Used as basis for all other entry types since they all
   --  include the same fields but interpret some in a different way. See Intel
   --  SDM Vol. 3A, page 4-28.
   type Table_Entry_Type is private;

   --  Returns True if entry is present in memory.
   function Is_Present (E : Table_Entry_Type) return Boolean;

   --  Returns True if entry is writable.
   function Is_Writable (E : Table_Entry_Type) return Boolean;

   --  Returns True if entry allows user-mode access.
   function Is_User_Accessible (E : Table_Entry_Type) return Boolean;

   --  Returns True if page-level write-through is enabled.
   function Is_Writethrough (E : Table_Entry_Type) return Boolean;

   --  Returns True if page-level cache is disabled.
   function Is_Cache_Disabled (E : Table_Entry_Type) return Boolean;

   --  Returns True if entry has been used for linear-address translation.
   function Was_Accessed (E : Table_Entry_Type) return Boolean;

   --  Returns True if execute-disable (NXE) is set, meaning instruction
   --  fetches are not allowed from the memory region controlled by this entry.
   function Has_Execute_Disable (E : Table_Entry_Type) return Boolean;

   --  Page Map Level 4 table entry, see Intel SDM Vol. 3A, page 4-28.
   subtype PML4_Entry_Type is Table_Entry_Type;

   --  Create a new PML4 entry with specified attributes.
   function Create_PML4_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return PML4_Entry_Type;

   --  Returns address pointed to by the given entry.
   function Get_PDPT_Address (E : PML4_Entry_Type) return SK.Word64;

   --  A Page Map Level 4 table comprises 512 64-bit entries (PML4Es), see
   --  Intel SDM Vol. 3A, page 4-22.
   type PML4_Table_Type is array (Table_Range) of PML4_Entry_Type;

   PML4_Null_Entry : constant PML4_Entry_Type;
   Null_PML4_Table : constant PML4_Table_Type;

   --  Directory entry type. Used as basis for all other directory entry types.
   subtype Directory_Entry_Type is Table_Entry_Type;

   --  Returns True if given directory entry has page attribute tables
   --  activated.
   function Has_PAT (E : Directory_Entry_Type) return Boolean;

   --  Returns True if given directory entry maps a physical page.
   function Maps_Page (E : Directory_Entry_Type) return Boolean;

   --  Page directory pointer table entry, see Intel SDM Vol. 3A, page 4-28.
   subtype PDPT_Entry_Type is Directory_Entry_Type;

   --  Create a new PDPT entry with specified attributes. The map page
   --  parameter specifies if the entry maps a 1 GB page.
   function Create_PDPT_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Map_Page      : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return PDPT_Entry_Type;

   --  Returns address pointed to by the given entry.
   function Get_PD_Address (E : PDPT_Entry_Type) return SK.Word64;

   --  A page-directory pointer table comprises 512 64-bit entries (PDPTEs),
   --  see Intel SDM Vol. 3A, page 4-22.
   type PDP_Table_Type is array (Table_Range) of PDPT_Entry_Type;

   PDPT_Null_Entry : constant PDPT_Entry_Type;
   Null_PDP_Table  : constant PDP_Table_Type;

   --  Page directory entry, see Intel SDM Vol. 3A, page 4-28.
   subtype PD_Entry_Type is Directory_Entry_Type;

   --  Create a new PD entry with specified attributes. The map page parameter
   --  specifies if the entry maps a 2 MB page.
   function Create_PD_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Map_Page      : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return PDPT_Entry_Type;

   --  Returns address pointed to by the given entry.
   function Get_PT_Address (E : PD_Entry_Type) return SK.Word64;

   --  A page directory comprises 512 64-bit entries (PDEs), see Intel SDM
   --  Vol. 3A, 4 - 22.
   type PD_Table_Type is array (Table_Range) of PD_Entry_Type;

   PD_Null_Entry : constant PD_Entry_Type;
   Null_PD_Table : constant PD_Table_Type;

   --  Page-table entry, see Intel SDM Vol. 3A, page 4-28.
   subtype PT_Entry_Type is Table_Entry_Type;

   --  Create a new PT entry with specified attributes.
   function Create_PT_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return PT_Entry_Type;

   --  Returns True if software has accessed the 4-KByte page referenced by the
   --  given entry.
   function Is_Dirty (E : PT_Entry_Type) return Boolean;

   --  Returns True if address translations using this entry are cached
   --  globally.
   function Is_Global (E : PT_Entry_Type) return Boolean;

   --  Returns True if given entry has corresponding page attribute table.
   function PT_Has_PAT (E : PT_Entry_Type) return Boolean;

   --  Returns address pointed to by the given entry.
   function Get_Address (E : PT_Entry_Type) return SK.Word64;

   --  A page table comprises 512 64-bit entries (PTEs), see Intel SDM Vol.
   --  3A, page 4-22.
   type Page_Table_Type is array (Table_Range) of PT_Entry_Type;

   PT_Null_Entry   : constant PT_Entry_Type;
   Null_Page_Table : constant Page_Table_Type;

   --  Return the paging structure indexes for a given linear address, see
   --  Intel SDM Vol. 3A, page 4-22:
   --   * PML4 index is formed by bits 39 .. 47
   --   * PDPT index is formed by bits 30 .. 38
   --   * PD   index is formed by bits 21 .. 29
   --   * PT   index is formed by bits 12 .. 20
   procedure Get_Indexes
     (Address    :     SK.Word64;
      PML4_Index : out Table_Range;
      PDPT_Index : out Table_Range;
      PD_Index   : out Table_Range;
      PT_Index   : out Table_Range);

private

   type Table_Entry_Type is new SK.Word64;

   PML4_Null_Entry : constant PML4_Entry_Type := 0;
   PDPT_Null_Entry : constant PDPT_Entry_Type := 0;
   PD_Null_Entry   : constant PD_Entry_Type   := 0;
   PT_Null_Entry   : constant PT_Entry_Type   := 0;

   Null_PML4_Table : constant PML4_Table_Type := (others => PML4_Null_Entry);
   Null_PDP_Table  : constant PDP_Table_Type  := (others => PDPT_Null_Entry);
   Null_PD_Table   : constant PD_Table_Type   := (others => PD_Null_Entry);
   Null_Page_Table : constant Page_Table_Type := (others => PT_Null_Entry);

end Skp.Paging;
