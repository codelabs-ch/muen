with Storage_Interface; use Storage_Interface;

package NVMe.SubmissionQ
is

   ---------------------------------------------------
   --- 3.3.3.1 Submission Queue Entry
   ---------------------------------------------------

   -- values are either address of PRP_Entry or Address of PRP_List
   type PRP_Data_Ptr is record
      E1 : Unsigned_64;
      E2 : Unsigned_64;
   end record
   with Size => 16 * 8;

   type PRP_List_Range is new Unsigned_8 range 0 .. 255;
   type PRP_List_Type is array (PRP_List_Range) of Unsigned_64
   with Pack, Object_Size => 64 * 256;

   for PRP_Data_Ptr use record
      E1 at 0 range  0 ..  63;
      E2 at 0 range 64 .. 127;
   end record;

   type SGL_Data_Ptr is
   record
      SGL : Unsigned_128;
   end record
   with Size => 16 * 8;

   for SGL_Data_Ptr use record
      SGL at 0 range 0 .. 127;
   end record;

   type SQE  is record
      OPC      : Unsigned_8;       -- -+
      FUSE     : Unsigned_2;       --  |
      Reserved : Bit_Array (2 .. 5); --| CDW0
      PSDT     : Unsigned_2;       --  |
      CID      : Unsigned_16;      -- -+

      NSID     : Unsigned_32;      -- -+ CDW1

      CDW2     : Unsigned_32;      -- -+ CDW2
      CDW3     : Unsigned_32;      -- -+ CDW3

      MPTR     : Unsigned_64;      -- -+ CDW4 .. CDW5

      DPRP     : PRP_Data_Ptr;

      CDW10    : Unsigned_32;
      CDW11    : Unsigned_32;
      CDW12    : Unsigned_32;
      CDW13    : Unsigned_32;
      CDW14    : Unsigned_32;
      CDW15    : Unsigned_32;

   end record
   with Size => 64 * 8;

   for SQE use record
      OPC          at  0 range 0 ..   7;
      FUSE         at  1 range 0 ..   1;
      Reserved     at  1 range 2 ..   5;
      PSDT         at  1 range 6 ..   7;
      CID          at  2 range 0 ..  15;

      NSID         at  4 range 0 ..  31;
      CDW2         at  8 range 0 ..  31;
      CDW3         at 12 range 0 ..  31;
      MPTR         at 16 range 0 ..  63;

      DPRP         at 24 range 0 .. 127;

      CDW10        at 40 range 0 ..  31;
      CDW11        at 44 range 0 ..  31;
      CDW12        at 48 range 0 ..  31;
      CDW13        at 52 range 0 ..  31;
      CDW14        at 56 range 0 ..  31;
      CDW15        at 60 range 0 ..  31;
   end record;

   Null_SQE : constant SQE :=
      (PSDT     => 0,
       OPC      => 0,
       FUSE     => 0,
       Reserved => (others => False),
       CID      => 0,
       NSID     => 0,
       CDW2     => 0,
       CDW3     => 0,
       MPTR     => 0,
       DPRP     => (others => 0),
       others   => 0);

   -------------------------------------------------------------------------
   --- Submission Queue
   -------------------------------------------------------------------------

   type Entry_Queue_Range is new Unsigned_6 range 0 .. 63;

   type Entry_Queue is array (Entry_Queue_Range) of SQE
   with Pack, Object_Size => 64 * 8 * 64;

   -------------------------------------------------------------------------

   subtype Admin_Command is SQE
   with Predicate => Admin_Command.PSDT = 0 and
                     Admin_Command.OPC in AdminCMD_Valid_Opcodes;

   subtype AdminCMD_Valid_Opcodes is Unsigned_8
   with Predicate => AdminCMD_Valid_Opcodes in
      0 .. 2 | 4 .. 6 | 8 .. 10 | 12 .. 13 | 16#10# .. 16#11# |
      16#14# .. 16#15# | 16#18# .. 16#1A# | 16#1C# .. 16#1E# |
      16#20# | 16#24# | 16#7C# | 16#7F# | 16#80# .. 16#82# |
      16#84# | 16#86#;

   subtype IO_Command is SQE
   with Predicate => IO_Command.PSDT = 0 and
                     IO_Command.OPC in IOCMD_Valid_Opcodes;

   subtype IOCMD_Valid_Opcodes is Unsigned_8
   with Predicate => IOCMD_Valid_Opcodes in
      0 .. 2 | 4 .. 5 | 8 .. 9 | 12 .. 14 | 16#11# | 16#15# | 16#19#;

end NVMe.SubmissionQ;
