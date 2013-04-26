with System.Machine_Code;
with System.Storage_Elements;

with SK.Descriptors;
with SK.IO;
with SK.Console;
with SK.Console_VGA;

package body SubjC
is

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

   --  Assembly ISR.
   ISR_Handler : SK.Word64;
   pragma Import (C, ISR_Handler, "isr_handler_ptr");

   IDT : SK.Descriptors.IDT_Type;

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   type GDT_Type is array (1 .. 3) of SK.Word64;

   GDT : GDT_Type;
   for GDT'Alignment use 8;

   --  Global descriptor table pointer, loaded into GDTR
   GDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   --  PS/2 constants.

   Data_Port       : constant := 16#60#;
   Status_Register : constant := 16#64#;

   OUTPUT_BUFFER_STATUS : constant := 0;

   type Scancode_Map is array (SK.Byte'Range) of Character;

   Char_Map : constant Scancode_Map
     := (2      => '1',
         3      => '2',
         4      => '3',
         5      => '4',
         6      => '5',
         7      => '6',
         8      => '7',
         9      => '8',
         10     => '9',
         11     => '0',
         12     => '-',
         13     => '=',
         16     => 'q',
         17     => 'w',
         18     => 'e',
         19     => 'r',
         20     => 't',
         21     => 'z',
         22     => 'u',
         23     => 'i',
         24     => 'o',
         25     => 'p',
         26     => '[',
         27     => ']',
         30     => 'a',
         31     => 's',
         32     => 'd',
         33     => 'f',
         34     => 'g',
         35     => 'h',
         36     => 'j',
         37     => 'k',
         38     => 'l',
         39     => ';',
         40     => ''',
         41     => '`',
         43     => ''',
         44     => 'y',
         45     => 'x',
         46     => 'c',
         47     => 'v',
         48     => 'b',
         49     => 'n',
         50     => 'm',
         51     => ',',
         52     => '.',
         53     => '-',
         55     => '*',
         57     => ' ',
         86     => '<',
         others => ' ');

   Ctrl : Boolean := False;

   --  Virtual text console framebuffer.
   type Framebuffer_Type is array (1 .. SK.Page_Size) of SK.Byte;
   for Framebuffer_Type'Size use 32768;

   --  Session slots.
   type Slot_Range is range 1 .. 4;

   --  Framebuffer base address.
   Framebuffer_Base : constant := 16#18000#;

   --  VGA output page.
   VGA_Out : Framebuffer_Type;
   for VGA_Out'Address use System'To_Address (16#000b_8000#);

   --  Update VGA console using the given slot framebuffer.
   procedure Update_Output (Slot : Slot_Range);

   -------------------------------------------------------------------------

   procedure Handle_Interrupt
   is
      use type SK.Byte;

      Status, Data : SK.Byte;
   begin
      loop
         SK.IO.Inb (Port  => Status_Register,
                    Value => Status);
         exit when not SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => Data_Port,
                    Value => Data);

         if Data <= 86 then
            case Data is
               when 1  =>
                  Text_IO.Init;
               when 15 =>
                  Text_IO.Put_String ("    ");
               when 28 =>
                  Text_IO.New_Line;
               when 29 =>
                  Ctrl := True;
               when 42 =>
                  --  Caps Lock or Shift pressed
                  null;
               when 56 =>
                  --  Alt pressed
                  null;
               when 58 =>
                  --  Ctrl pressed
                  Ctrl := not Ctrl;
               when 59 =>
                  Update_Output (Slot => 1);
               when 60 =>
                  Text_IO.Put_String (Item => " F2 ");
               when 61 =>
                  Text_IO.Put_String (Item => " F3 ");
               when 62 =>
                  Text_IO.Put_String (Item => " F4 ");
               when others =>
                  Text_IO.Put_Char (Item => Char_Map (Data));
            end case;
         end if;

         if Data = 157 then
            --  Ctrl released
            Ctrl := False;
         elsif Data = 170 then
            --  Shift released
            null;
         elsif Data = 184 then
            --  Alt released
            null;
         end if;
      end loop;
   end Handle_Interrupt;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      use type SK.Word16;
      use type SK.Word64;
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

      IDT := SK.Descriptors.IDT_Type'
        (others => SK.Descriptors.Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in SK.Descriptors.Vector_Type range IDT'Range loop
         IDT (I) := SK.Descriptors.Gate_Type'
           (Offset_15_00     => SK.Word16
              (ISR_Handler and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((ISR_Handler and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32
              ((ISR_Handler and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;

      IDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (IDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);

      Text_IO.Init;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Update_Output (Slot : Slot_Range)
   is
      Fb : Framebuffer_Type;
      for Fb'Address use System'To_Address
        (Framebuffer_Base + SK.Page_Size * Natural (Slot - 1));
   begin
      loop
         for I in Fb'Range loop
            VGA_Out (I) := Fb (I);
         end loop;
      end loop;
   end Update_Output;

begin
   System.Machine_Code.Asm
     (Template => "cli",
      Volatile => True);
end SubjC;
