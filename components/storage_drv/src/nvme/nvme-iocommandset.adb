with Ada.Unchecked_Conversion;
with System;

package body NVMe.IOCommandSet
is

   ----------------------------
   -- Named Address Numbers
   ----------------------------
   -- Queue Memory
   PRP_List_Offset  : constant := 16#0009_0000#;
   PRP_List_Address : constant := NVMe.Host.DRAM_Memory.Queue_Memory_Address + PRP_List_Offset;

   -------------------------------------------------------------------------

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   -- Due to Muenblock, there will not be a request with more than a 254 NLB aka 255 count
   PRP_List : NVMe.SubmissionQ.PRP_List_Type := (others => 0)
   with
      Volatile, Async_Readers,
      Address => System'To_Address (PRP_List_Address);

   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   -------------------------------------------------------------------------

   -- Check if PRP.E2 is needed and fill it accordingly with PRP Entry or PRP List
   -- this only works if DMA is physically continuous
   procedure Correct_PRP
      (PRP           :     SubmissionQ.PRP_Data_Ptr;
       NLB           :     Unsigned_16;
       Corrected_PRP : out SubmissionQ.PRP_Data_Ptr)
   with Pre => (NVMe.Host.Is_Valid)
   is
      use type SubmissionQ.PRP_List_Range;

      Sector_Size       : constant Unsigned_64 := Unsigned_64 (Host.Get_Sector_Size);
      Exponent          : constant Natural     := 12 + Natural (NVMe.Host.Memory_Page_Size);
      MPS_UInt          : constant Unsigned_64 := Unsigned_64 (2) ** Exponent;
      MPS_Int           : constant Integer_64  := Integer_64 (MPS_UInt);
      Amount_Data       : constant Unsigned_64 := Unsigned_64 (NLB) * Sector_Size;
      Remaining_Data    : Integer_64;
      Number_PRPs       : Unsigned_8;
      PRP_Base          : Unsigned_64;

   begin
      Corrected_PRP := PRP;

      -- Calculating the Data that could not fit into PRP.E1 (with the given Offset)

      if Amount_Data > (MPS_UInt - (PRP.E1 and (MPS_UInt - 1))) then
         Remaining_Data := Integer_64 (Amount_Data) - (MPS_Int - Integer_64 (PRP.E1 and (MPS_UInt - 1)));

         pragma Assert (MPS_Int > 0);
         Number_PRPs := Unsigned_8'Mod (Remaining_Data / MPS_Int) + (if Remaining_Data rem MPS_Int > 0 then 1 else 0);

         PRP_Base := PRP.E1 - (PRP.E1 and (MPS_UInt - 1));

         if Number_PRPs <= 1 then
            Corrected_PRP.E2 :=  PRP_Base + MPS_UInt;
         else
            -- prp list needed
            -- reset PRP List
            PRP_List := (others => 0);
            -- fill PRP List
            for Index in SubmissionQ.PRP_List_Range range 1 .. SubmissionQ.PRP_List_Range (Number_PRPs) loop
               PRP_List (Index - 1) := PRP_Base + (Unsigned_64 (Index) * MPS_UInt);
            end loop;

            Corrected_PRP.E2 := PRP_List_Address;
         end if;
      end if;
   end Correct_PRP;

   -------------------------------------------------------------------------

   procedure CreateRead_Command
      (CMD_Identifier  : in out Unsigned_16;              -- Command Identifier
       DPTR            :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       SLBA            :        Unsigned_64;              -- Starting Logical Block Address (LBA)
       NLB             :        Unsigned_16;              -- Number of Logical Blocks
       Command         :    out SubmissionQ.IO_Command)
   is
      CDW10and11_Temp : CDW10and11_RW;
      PRP : SubmissionQ.PRP_Data_Ptr;

      function Cvt_to_CDW10and11 is new Ada.Unchecked_Conversion (Unsigned_64, CDW10and11_RW);
      function Cvt_CDW12         is new Ada.Unchecked_Conversion (CDW12_Read, Unsigned_32);
      function Cvt_CDW13         is new Ada.Unchecked_Conversion (CDW13_Read, Unsigned_32);

   begin

      CDW10and11_Temp := Cvt_to_CDW10and11 (SLBA);
      Correct_PRP (DPTR, NLB + 1, PRP); -- +1 to undo zero based NLB

      Command :=
         (PSDT  => 0,
          OPC   => 2,
          FUSE  => 0,
          Reserved => (others => False),
          CID   => CMD_Identifier,
          NSID  => 1,
          CDW2  => 0,
          CDW3  => 0,
          MPTR  => 0,
          CDW10 => CDW10and11_Temp.CDW10,
          CDW11 => CDW10and11_Temp.CDW11,
          CDW12 => Cvt_CDW12 ((NLB => NLB, STC => False, PRINFO => 0, FUA => True, LR => False, others => <>)),
          CDW13 => Cvt_CDW13 (((DSM => (Access_Frequency => 1, Access_Latency => 0, Sequential_Request => False, Incompressible => True), others => <>))),
          CDW14 => 0,
          CDW15 => 0,
          DPRP  => PRP);

      CMD_Identifier := CMD_Identifier + 1;

   end CreateRead_Command;

   -------------------------------------------------------------------------

   procedure CreateWrite_Command
      (CMD_Identifier  : in out Unsigned_16;              -- Command Identifier
       DPTR            :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       SLBA            :        Unsigned_64;              -- Starting Logical Block Address (LBA)
       NLB             :        Unsigned_16;              -- Number of Logical Blocks
       Command         :    out SubmissionQ.IO_Command)
   is
      CDW10and11_Temp : CDW10and11_RW;
      PRP             : SubmissionQ.PRP_Data_Ptr;

      function Cvt_to_CDW10and11 is new Ada.Unchecked_Conversion (Unsigned_64, CDW10and11_RW);
      function Cvt_CDW12         is new Ada.Unchecked_Conversion (CDW12_Write, Unsigned_32);
      function Cvt_CDW13         is new Ada.Unchecked_Conversion (CDW13_Write, Unsigned_32);

   begin

      CDW10and11_Temp := Cvt_to_CDW10and11 (SLBA);
      Correct_PRP (DPTR, NLB + 1, PRP);

      Command :=
         (PSDT  => 0,
          OPC   => 1,
          FUSE  => 0,
          Reserved => (others => False),
          CID   => CMD_Identifier,
          NSID  => 1,
          CDW2  => 0,
          CDW3  => 0,
          MPTR  => 0,
          CDW10 => CDW10and11_Temp.CDW10,
          CDW11 => CDW10and11_Temp.CDW11,
          CDW12 => Cvt_CDW12 ((NLB => NLB, DTYPE => 0, STC => False, PRINFO => 0, FUA => True, LR => False, others => <>)),
          CDW13 => Cvt_CDW13 (
             (DSM => (Access_Frequency => 1, Access_Latency => 0, Sequential_Request => False, Incompressible => True),
              DSPEC => 0, Filler => 0)),
          CDW14 => 0,
          CDW15 => 0,
          DPRP  => PRP);

      CMD_Identifier := CMD_Identifier + 1;

   end CreateWrite_Command;

   -------------------------------------------------------------------------

   procedure CreateWrite_Zeroes_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       SLBA            :        Unsigned_64;          -- Starting Logical Block Address (LBA)
       NLB             :        Unsigned_16;          -- Number of Logical Blocks
       Command         :    out SubmissionQ.IO_Command)
   is
      CDW10and11_Temp : CDW10and11_RW;

      function Cvt_CDW12 is new Ada.Unchecked_Conversion (CDW12_Write_Zeroes, Unsigned_32);
      function Cvt_to_CDW10and11 is new Ada.Unchecked_Conversion (Unsigned_64, CDW10and11_RW);

   begin

      CDW10and11_Temp := Cvt_to_CDW10and11 (SLBA);

      Command :=
         (PSDT  => 0,
          OPC   => 8,
          FUSE  => 0,
          Reserved => (others => False),
          CID   => CMD_Identifier,
          NSID  => 1,
          CDW2  => 0,
          CDW3  => 0,
          MPTR  => 0,
          CDW10 => CDW10and11_Temp.CDW10,
          CDW11 => CDW10and11_Temp.CDW11,
          CDW12 => Cvt_CDW12 ((NLB => NLB, STC => False, DEAC => True, PRINFO => 0, FUA => True, LR => False, others => <>)),
          CDW13 => 0,
          CDW14 => 0,
          CDW15 => 0,
          DPRP  => (0, 0));

      CMD_Identifier := CMD_Identifier + 1;

   end CreateWrite_Zeroes_Command;

   -------------------------------------------------------------------------

   procedure CreateFlush_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       Command         :    out SubmissionQ.IO_Command)
   is
   begin

      Command :=
         (PSDT  => 0,
          OPC   => 0,
          FUSE  => 0,
          Reserved => (others => False),
          CID   => CMD_Identifier,
          NSID  => 16#FFFFFFFF#,
          CDW2  => 0,
          CDW3  => 0,
          MPTR  => 0,
          CDW10 => 0,
          CDW11 => 0,
          CDW12 => 0,
          CDW13 => 0,
          CDW14 => 0,
          CDW15 => 0,
          DPRP  => (0, 0));

      CMD_Identifier := CMD_Identifier + 1;

   end CreateFlush_Command;
end NVMe.IOCommandSet;
