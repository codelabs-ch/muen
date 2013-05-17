package body Skp.Paging.EPT
is

   Read_Flag       : constant := 0;
   Write_Flag      : constant := 1;
   Execute_Flag    : constant := 2;
   Ignore_PAT_Flag : constant := 6;

   --  Mapping of memory type to EPT memory type bits, see Intel SDM Vol. 3C,
   --  chapter 28.2.5.
   EPT_MT_Mapping : constant array (Memory_Type_Type) of PT_Entry_Type
     := (UC => 16#00#,
         WC => 16#08#,
         WT => 16#20#,
         WP => 16#28#,
         WB => 16#30#);

   --  Create EPT paging structure entry with specified parameters.
   generic
      type Entry_Type is new Table_Entry_Type;
   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Entry_Type;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Entry_Type
   is
      New_Entry : Table_Entry_Type;
   begin
      New_Entry := Table_Entry_Type (Address) and Address_Mask;

      if Readable then
         Set_Flag (E    => New_Entry,
                   Flag => Read_Flag);
      end if;

      if Writable then
         Set_Flag (E    => New_Entry,
                   Flag => Write_Flag);
      end if;

      if Executable then
         Set_Flag (E    => New_Entry,
                   Flag => Execute_Flag);
      end if;

      return Entry_Type (New_Entry);
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_PD is new Create_Entry (Entry_Type => PD_Entry_Type);
   function Create_PD_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PD_Entry_Type renames Create_PD;

   -------------------------------------------------------------------------

   function Create_PDPT is new Create_Entry (Entry_Type => PDPT_Entry_Type);
   function Create_PDPT_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PDPT_Entry_Type renames Create_PDPT;

   -------------------------------------------------------------------------

   function Create_PML4 is new Create_Entry (Entry_Type => PML4_Entry_Type);
   function Create_PML4_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PML4_Entry_Type renames Create_PML4;

   -------------------------------------------------------------------------

   function Create_PT_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Memory_Type_Type)
      return PT_Entry_Type
   is
      function Create_PT is new Create_Entry (Entry_Type => PT_Entry_Type);

      New_Entry : PT_Entry_Type;
   begin
      New_Entry := Create_PT (Address    => Address,
                              Readable   => Readable,
                              Writable   => Writable,
                              Executable => Executable);

      if Ignore_PAT then
         Set_Flag (E    => Table_Entry_Type (New_Entry),
                   Flag => Ignore_PAT_Flag);
      end if;

      New_Entry := New_Entry or EPT_MT_Mapping (Memory_Type);

      return New_Entry;
   end Create_PT_Entry;

end Skp.Paging.EPT;
