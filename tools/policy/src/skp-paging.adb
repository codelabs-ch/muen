package body Skp.Paging
is

   Present_Flag   : constant := 0;
   RW_Flag        : constant := 1;
   US_Flag        : constant := 2;
   PWT_Flag       : constant := 3;
   PCD_Flag       : constant := 4;
   Page_Size_Flag : constant := 7;
   PTE_PAT_Flag   : constant := 7;
   Global_Flag    : constant := 8;
   PD_PAT_Flag    : constant := 12;
   NXE_Flag       : constant := 63;

   --  Table entry address range is bits 12 .. 47.
   Address_Mask : constant Table_Entry_Type := 16#0000fffffffff000#;

   --  PDPTE address range is bits 30 .. 47; entry maps a 1 GB page.
   PDPT_Address_Mask : constant PDPT_Entry_Type := 16#0000ffffc0000000#;

   --  PDE address range is bits 21 .. 47; entry maps a 2 MB page.
   PD_Address_Mask : constant PD_Entry_Type := 16#0000ffffffe00000#;

   --  PML4 index is bits 39 .. 47 of the linear address.
   PML4_Index_Mask : constant SK.Word64 := 16#0000ff8000000000#;

   --  PDPT index is bits 30 .. 38 of the linear address.
   PDPT_Index_Mask : constant SK.Word64 := 16#0000007fc0000000#;

   --  PD index is bits 21 .. 29 of the linear address.
   PD_Index_Mask : constant SK.Word64 := 16#000000003fe00000#;

   --  PT index is bits 12 .. 20 of the linear address.
   PT_Index_Mask : constant SK.Word64 := 16#00000000001ff000#;

   --  Set specified flag.
   procedure Set_Flag
     (E    : in out Table_Entry_Type;
      Flag :        SK.Word64_Pos);

   --  Create paging structure entry with specified parameters.
   function Create_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Table_Entry_Type;

   --  Create paging directory entry (PDPT or PD) with specified parameters.
   function Create_Directory_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Map_Page      : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return Directory_Entry_Type;

   -------------------------------------------------------------------------

   function Create_Directory_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Map_Page      : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return Directory_Entry_Type
   is
      DE : Directory_Entry_Type;
   begin
      DE := Create_Entry (Address       => Address,
                          Writable      => Writable,
                          User_Access   => User_Access,
                          Writethrough  => Writethrough,
                          Cache_Disable => Cache_Disable,
                          Exec_Disable  => Exec_Disable);

      if Map_Page then
         Set_Flag (E    => DE,
                   Flag => Page_Size_Flag);

         if PAT then
            Set_Flag (E    => DE,
                      Flag => PD_PAT_Flag);
         end if;

         if Global then
            Set_Flag (E    => DE,
                      Flag => Global_Flag);
         end if;
      end if;

      return DE;
   end Create_Directory_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Table_Entry_Type
   is
      New_Entry : Table_Entry_Type;
   begin
      New_Entry := Table_Entry_Type (Address) and Address_Mask;

      Set_Flag (E    => New_Entry,
                Flag => Present_Flag);

      if Writable then
         Set_Flag (E    => New_Entry,
                   Flag => RW_Flag);
      end if;

      if User_Access then
         Set_Flag (E    => New_Entry,
                   Flag => US_Flag);
      end if;

      if Writethrough then
         Set_Flag (E    => New_Entry,
                   Flag => PWT_Flag);
      end if;

      if Cache_Disable then
         Set_Flag (E    => New_Entry,
                   Flag => PCD_Flag);
      end if;

      if Exec_Disable then
         Set_Flag (E    => New_Entry,
                   Flag => NXE_Flag);
      end if;

      return New_Entry;
   end Create_Entry;

   -------------------------------------------------------------------------

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
      return PD_Entry_Type renames Create_Directory_Entry;

   -------------------------------------------------------------------------

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
      return PDPT_Entry_Type renames Create_Directory_Entry;

   -------------------------------------------------------------------------

   function Create_PML4_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return PML4_Entry_Type
   is
   begin
      return PML4_Entry_Type
        (Create_Entry (Address       => Address,
                       Writable      => Writable,
                       User_Access   => User_Access,
                       Writethrough  => Writethrough,
                       Cache_Disable => Cache_Disable,
                       Exec_Disable  => Exec_Disable));
   end Create_PML4_Entry;

   -------------------------------------------------------------------------

   function Create_PT_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Global        : Boolean;
      PAT           : Boolean;
      Exec_Disable  : Boolean)
      return PT_Entry_Type
   is
      PTE : PT_Entry_Type;
   begin
      PTE := Create_Entry (Address       => Address,
                           Writable      => Writable,
                           User_Access   => User_Access,
                           Writethrough  => Writethrough,
                           Cache_Disable => Cache_Disable,
                           Exec_Disable  => Exec_Disable);

      if PAT then
         Set_Flag (E    => PTE,
                   Flag => PTE_PAT_Flag);
      end if;

      if Global then
         Set_Flag (E    => PTE,
                   Flag => Global_Flag);
      end if;

      return PTE;
   end Create_PT_Entry;

   -------------------------------------------------------------------------

   function Get_Address (E : Table_Entry_Type) return SK.Word64
   is
   begin
      return SK.Word64 (E and Address_Mask);
   end Get_Address;

   -------------------------------------------------------------------------

   procedure Get_Indexes
     (Address    :     SK.Word64;
      PML4_Index : out Table_Range;
      PDPT_Index : out Table_Range;
      PD_Index   : out Table_Range;
      PT_Index   : out Table_Range)
   is
      use type SK.Word64;
   begin

      --  Add 1 since the table range starts at 1 instead of 0.

      PML4_Index := Table_Range ((Address and PML4_Index_Mask) / 2 ** 39 + 1);
      PDPT_Index := Table_Range ((Address and PDPT_Index_Mask) / 2 ** 30 + 1);
      PD_Index   := Table_Range ((Address and PD_Index_Mask) / 2 ** 21 + 1);
      PT_Index   := Table_Range ((Address and PT_Index_Mask) / 2 ** 12 + 1);
   end Get_Indexes;

   -------------------------------------------------------------------------

   function Get_PD_Address (E : PDPT_Entry_Type) return SK.Word64
   is
      Address : SK.Word64;
   begin
      if SK.Bit_Test
        (Value => SK.Word64 (E),
         Pos   => Page_Size_Flag)
      then
         Address := SK.Word64 (E and PDPT_Address_Mask);
      else
         Address := SK.Word64 (E and Address_Mask);
      end if;

      return Address;
   end Get_PD_Address;

   -------------------------------------------------------------------------

   function Get_PDPT_Address
     (E : PML4_Entry_Type) return SK.Word64
   is
   begin
      return Get_Address (E => Table_Entry_Type (E));
   end Get_PDPT_Address;

   -------------------------------------------------------------------------

   function Get_PT_Address (E : PD_Entry_Type) return SK.Word64
   is
      Address : SK.Word64;
   begin
      if SK.Bit_Test
        (Value => SK.Word64 (E),
         Pos   => Page_Size_Flag)
      then
         Address := SK.Word64 (E and PD_Address_Mask);
      else
         Address := SK.Word64 (E and Address_Mask);
      end if;

      return Address;
   end Get_PT_Address;

   -------------------------------------------------------------------------

   procedure Set_Flag
     (E    : in out Table_Entry_Type;
      Flag :        SK.Word64_Pos)
   is
   begin
      E := Table_Entry_Type
        (SK.Bit_Set (Value => SK.Word64 (E),
                     Pos   => Flag));
   end Set_Flag;

end Skp.Paging;
