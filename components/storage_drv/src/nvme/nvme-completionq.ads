with Storage_Interface; use Storage_Interface;

package NVMe.CompletionQ
is

   -------------------------------------------------------------------------
   --- 3.3.3.2 Common Completion Queue Entry
   -------------------------------------------------------------------------

   type StatusField is record
      SC  : Unsigned_8; -- Status Code
      SCT : Unsigned_3; -- Status Code Type
      CRD : Unsigned_2; -- Command Retry Delay
      M   : Boolean;    -- More (Information - see Error Page)
      DNR : Boolean;    -- Do Not Retry (bc expected to fail)
   end record with
     Size => 15;

   for StatusField use record
      SC  at 0 range 0 .. 7;
      SCT at 1 range 0 .. 2;
      CRD at 1 range 3 .. 4;
      M   at 1 range 5 .. 5;
      DNR at 1 range 6 .. 6;
   end record;

   Null_StatusField : StatusField := (SC  => 0,
                                      SCT => 0,
                                      CRD => 0,
                                      others   => False);

   type CQE is record
      DWORD0 : Unsigned_32;
      DWORD1 : Unsigned_32;

      SQHD   : Unsigned_16;      -- Submission Queue Head Pointer
      SQID   : Unsigned_16;      -- Submission Queue Identifier

      CID    : Unsigned_16;      -- Command Identifier
      P      : Boolean;          -- Phase Tag
      Status : StatusField;      -- Status Field
   end record with
     Size => 16 * 8;

   for CQE use record
      DWORD0 at  0 range 0 .. 31;
      DWORD1 at  4 range 0 .. 31;
      SQHD   at  8 range 0 .. 15;
      SQID   at 10 range 0 .. 15;
      CID    at 12 range 0 .. 15;
      P      at 14 range 0 ..  0;
      Status at 14 range 1 .. 15;
   end record;

   Null_CQE : constant CQE := (P      => False,
                               Status => Null_StatusField,
                               DWORD0 => 0, DWORD1 => 0,
                               others => 0);

   -------------------------------------------------------------------------
   --- Completion QUEUE
   -------------------------------------------------------------------------

   type Entry_Queue_Range is new Unsigned_8 range 0 .. 255;

   type Entry_Queue is array (Entry_Queue_Range) of CQE
   with Pack, Object_Size => 256 * 16 * 8;

end NVMe.CompletionQ;
