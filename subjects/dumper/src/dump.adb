with System.Machine_Code;
with System.Storage_Elements;

with Skp;

with SK.Descriptors;
with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with Dumper_Kernel_Iface;

package body Dump
is

   --  ISR list type.
   type ISR_List_Type is array (SK.Descriptors.Vector_Type) of SK.Word64;

   --  ISR trampoline list.
   ISRs : ISR_List_Type;
   pragma Import (C, ISRs, "isrlist");
   --# assert ISR_List'Always_Valid;

   --  IDT, see Intel SDM Vol. 3A, chapter 6.10.
   IDT : SK.Descriptors.IDT_Type;

   type GDT_Type is array (1 .. 3) of SK.Word64;

   GDT : GDT_Type;
   for GDT'Alignment use 8;

   --  Global descriptor table pointer, loaded into GDTR
   GDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   package DKI renames Dumper_Kernel_Iface;

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   --  Load GDT with two entries (code & stack) and load it into GDT register.
   procedure Load_GDT;

   --  Load IDT into IDT register.
   procedure Load_IDT (IDT : SK.Descriptors.IDT_Type);

   --  Setup IDT using the given ISR list.
   procedure Setup_IDT
     (ISR_List :     ISR_List_Type;
      IDT      : out SK.Descriptors.IDT_Type);

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
      use SK;

      Id    : constant Integer := Integer (Vector) - 32;
      State : SK.Subject_State_Type;
   begin
      if Id in Skp.Subject_Id_Type'Range then
         State := DKI.Get_Subject_State (Id => Id);
         Text_IO.Put_String (Item => "Subject ");
         Text_IO.Put_Byte   (Item => Byte (Id));
         Text_IO.Put_String (Item => " EXIT (");
         Text_IO.Put_Word16 (Item => Word16 (State.Exit_Reason));
         Text_IO.Put_String (Item => ":");
         Text_IO.Put_Word32 (Item => Word32 (State.Exit_Qualification));
         Text_IO.Put_String (Item => ":");
         Text_IO.Put_Word32 (Item => Word32 (State.Interrupt_Info));
         Text_IO.Put_Line   (Item => ")");

         Text_IO.Put_String ("RIP: ");
         Text_IO.Put_Word64 (Item => State.RIP);
         Text_IO.Put_String (" CS : ");
         Text_IO.Put_Word16 (Item => Word16 (State.CS));
         Text_IO.Put_String (" RFLAGS: ");
         Text_IO.Put_Word32 (Item => Word32 (State.RFLAGS));
         Text_IO.New_Line;
         Text_IO.Put_String ("RSP: ");
         Text_IO.Put_Word64 (Item => State.RSP);
         Text_IO.Put_String (" SS : ");
         Text_IO.Put_Word16 (Item => Word16 (State.SS));
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "RAX: ");
         Text_IO.Put_Word64 (Item => State.Regs.RAX);
         Text_IO.Put_String (Item => " RBX: ");
         Text_IO.Put_Word64 (Item => State.Regs.RBX);
         Text_IO.Put_String (Item => " RCX: ");
         Text_IO.Put_Word64 (Item => State.Regs.RCX);
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "RDX: ");
         Text_IO.Put_Word64 (Item => State.Regs.RDX);
         Text_IO.Put_String (Item => " RSI: ");
         Text_IO.Put_Word64 (Item => State.Regs.RSI);
         Text_IO.Put_String (Item => " RDI: ");
         Text_IO.Put_Word64 (Item => State.Regs.RDI);
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "RBP: ");
         Text_IO.Put_Word64 (Item => State.Regs.RBP);
         Text_IO.Put_String (Item => " R08: ");
         Text_IO.Put_Word64 (Item => State.Regs.R08);
         Text_IO.Put_String (Item => " R09: ");
         Text_IO.Put_Word64 (Item => State.Regs.R09);
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "R10: ");
         Text_IO.Put_Word64 (Item => State.Regs.R10);
         Text_IO.Put_String (Item => " R11: ");
         Text_IO.Put_Word64 (Item => State.Regs.R11);
         Text_IO.Put_String (Item => " R12: ");
         Text_IO.Put_Word64 (Item => State.Regs.R12);
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "R13: ");
         Text_IO.Put_Word64 (Item => State.Regs.R13);
         Text_IO.Put_String (Item => " R14: ");
         Text_IO.Put_Word64 (Item => State.Regs.R14);
         Text_IO.Put_String (Item => " R15: ");
         Text_IO.Put_Word64 (Item => State.Regs.R15);
         Text_IO.New_Line;

         Text_IO.Put_String (Item => "CR0: ");
         Text_IO.Put_Word64 (Item => State.CR0);
         Text_IO.Put_String (Item => " CR3: ");
         Text_IO.Put_Word64 (Item => State.CR3);
         Text_IO.Put_String (Item => " CR4: ");
         Text_IO.Put_Word64 (Item => State.CR4);

         Hypercall.Swap_Relaunch (Subject_Id => Byte (Id));
      else
         Text_IO.Put_String (Item => "Dump requested for invalid subject id ");
         Text_IO.Put_Byte  (Item => Vector);
         Text_IO.New_Line;
      end if;
   end Handle_Interrupt;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      Setup_IDT (ISR_List => ISRs,
                 IDT      => IDT);
      Load_IDT (IDT => IDT);
      Load_GDT;
      Text_IO.Init;
      Text_IO.Put_Line ("Dumper subject running");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_GDT
   is
      use type SK.Word16;
   begin
      GDT := GDT_Type'(1 => 0,
                       2 => 16#20980000000000#,
                       3 => 16#20930000000000#);
      GDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (GDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => GDT'Address)));
      System.Machine_Code.Asm
        (Template => "lgdt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", GDT_Pointer'Address)),
         Volatile => True);
   end Load_GDT;

   -------------------------------------------------------------------------

   procedure Load_IDT (IDT : SK.Descriptors.IDT_Type)
   is
      use type SK.Word16;

      IDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;
   begin
      IDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (IDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);
   end Load_IDT;

   -------------------------------------------------------------------------

   procedure Setup_IDT
     (ISR_List :     ISR_List_Type;
      IDT      : out SK.Descriptors.IDT_Type)
   is
      use type SK.Word64;

      Temp : SK.Word64;
   begin
      IDT := SK.Descriptors.IDT_Type'
        (others => SK.Descriptors.Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in SK.Descriptors.Vector_Type range IDT'Range loop
         Temp := ISR_List (I);

         IDT (I) := SK.Descriptors.Gate_Type'
           (Offset_15_00     => SK.Word16
              (Temp and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((Temp and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32
              ((Temp and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;
   end Setup_IDT;

end Dump;
