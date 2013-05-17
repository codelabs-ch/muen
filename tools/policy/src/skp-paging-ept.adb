package body Skp.Paging.EPT
is

   Read_Flag    : constant := 0;
   Write_Flag   : constant := 1;
   Execute_Flag : constant := 2;

   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Table_Entry_Type;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Table_Entry_Type
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

      return New_Entry;
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_PD_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PD_Entry_Type
   is
   begin
      return PD_Entry_Type
        (Create_Entry (Address    => Address,
                       Readable   => Readable,
                       Writable   => Writable,
                       Executable => Executable));
   end Create_PD_Entry;

   -------------------------------------------------------------------------

   function Create_PDPT_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PDPT_Entry_Type
   is
   begin
      return PDPT_Entry_Type
        (Create_Entry (Address    => Address,
                       Readable   => Readable,
                       Writable   => Writable,
                       Executable => Executable));
   end Create_PDPT_Entry;

   -------------------------------------------------------------------------

   function Create_PML4_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PML4_Entry_Type
   is
   begin
      return PML4_Entry_Type
        (Create_Entry (Address    => Address,
                       Readable   => Readable,
                       Writable   => Writable,
                       Executable => Executable));
   end Create_PML4_Entry;

end Skp.Paging.EPT;
