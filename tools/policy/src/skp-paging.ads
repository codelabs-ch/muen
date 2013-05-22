with SK;

package Skp.Paging
is

   --  Implementation of IA-32e paging structures, as specified by Intel SDM
   --  Vol. 3A, section 4.5.

   --  All paging structure types (PML4, PDPT, PD, PT) have 512 entries.
   type Table_Range is range 0 .. 511;

   --  Table entry type. Used as basis for all other entry types since they all
   --  include the same fields but interpret some in a different way. See Intel
   --  SDM Vol. 3A, page 4-28.
   type Table_Entry_Type is new SK.Word64;

   --  Page Map Level 4 table entry, see Intel SDM Vol. 3A, page 4-28.
   type PML4_Entry_Type is new Table_Entry_Type;

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

   --  Page directory pointer table entry, see Intel SDM Vol. 3A, page 4-28.
   type PDPT_Entry_Type is new Table_Entry_Type;

   --  Create a new PDPT entry with specified attributes. The map page
   --  parameter specifies if the entry maps a 1 GB page.
   function Create_PDPT_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Memory_Type_Type;
      Exec_Disable : Boolean)
      return PDPT_Entry_Type;

   --  Returns address pointed to by the given entry.
   function Get_PD_Address (E : PDPT_Entry_Type) return SK.Word64;

   --  A page-directory pointer table comprises 512 64-bit entries (PDPTEs),
   --  see Intel SDM Vol. 3A, page 4-22.
   type PDP_Table_Type is array (Table_Range) of PDPT_Entry_Type;

   PDPT_Null_Entry : constant PDPT_Entry_Type;
   Null_PDP_Table  : constant PDP_Table_Type;

   --  Page directory entry, see Intel SDM Vol. 3A, page 4-28.
   type PD_Entry_Type is new Table_Entry_Type;

   --  Create a new PD entry with specified attributes. The map page parameter
   --  specifies if the entry maps a 2 MB page.
   function Create_PD_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Memory_Type_Type;
      Exec_Disable : Boolean)
      return PD_Entry_Type;

   --  Returns address pointed to by the given entry.
   function Get_PT_Address (E : PD_Entry_Type) return SK.Word64;

   --  A page directory comprises 512 64-bit entries (PDEs), see Intel SDM
   --  Vol. 3A, 4 - 22.
   type PD_Table_Type is array (Table_Range) of PD_Entry_Type;

   PD_Null_Entry : constant PD_Entry_Type;
   Null_PD_Table : constant PD_Table_Type;

   --  Page-table entry, see Intel SDM Vol. 3A, page 4-28.
   type PT_Entry_Type is new Table_Entry_Type;

   --  Create a new PT entry with specified attributes.
   function Create_PT_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Global       : Boolean;
      Memory_Type  : Memory_Type_Type;
      Exec_Disable : Boolean)
      return PT_Entry_Type;

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

   --  Set specified flag.
   procedure Set_Flag
     (E    : in out Table_Entry_Type;
      Flag :        SK.Word64_Pos);

   --  Table entry address range is bits 12 .. 47.
   Address_Mask : constant Table_Entry_Type := 16#0000fffffffff000#;

   PML4_Null_Entry : constant PML4_Entry_Type := 0;
   PDPT_Null_Entry : constant PDPT_Entry_Type := 0;
   PD_Null_Entry   : constant PD_Entry_Type   := 0;
   PT_Null_Entry   : constant PT_Entry_Type   := 0;

   Null_PML4_Table : constant PML4_Table_Type := (others => PML4_Null_Entry);
   Null_PDP_Table  : constant PDP_Table_Type  := (others => PDPT_Null_Entry);
   Null_PD_Table   : constant PD_Table_Type   := (others => PD_Null_Entry);
   Null_Page_Table : constant Page_Table_Type := (others => PT_Null_Entry);

end Skp.Paging;
