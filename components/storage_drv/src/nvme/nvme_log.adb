with Log;

package body NVMe_Log
is

   procedure Put_NVMe_AdminCMD_Image (TypeID : Interfaces.Unsigned_8)
   is
   begin
      case TypeID is
         when 16#00# => Log.Put_String ("Delete_IO_SQ");
         when 16#01# => Log.Put_String ("Create_IO_SQ");
         when 16#02# => Log.Put_String ("Get_Log_Page");
         when 16#04# => Log.Put_String ("Delete_IO_CQ");
         when 16#05# => Log.Put_String ("Create_IO_CQ");
         when 16#06# => Log.Put_String ("Identify");
         when 16#08# => Log.Put_String ("Abort_CMD");
         when 16#09# => Log.Put_String ("Set_Features");
         when 16#0A# => Log.Put_String ("Get_Features");
         when 16#0C# => Log.Put_String ("Async_Event_Req");
         when 16#0D# => Log.Put_String ("Namespace_Mngmt");
         when 16#10# => Log.Put_String ("Firmware_Commit");
         when 16#11# => Log.Put_String ("Firmware_Img_Download");
         when 16#14# => Log.Put_String ("Device_Self_Test");
         when 16#15# => Log.Put_String ("Namespace_Attachment");
         when 16#18# => Log.Put_String ("Keep_Alive");
         when 16#19# => Log.Put_String ("Directive_Send");
         when 16#1A# => Log.Put_String ("Directive_Receive");
         when 16#1C# => Log.Put_String ("Virtualization_Mngmt");
         when 16#1D# => Log.Put_String ("NVMe_MI_Send");
         when 16#1E# => Log.Put_String ("NVMe_MI_Receive");
         when 16#20# => Log.Put_String ("Capacity_Mngmt");
         when 16#24# => Log.Put_String ("Lockdown");
         when 16#7C# => Log.Put_String ("Doorbell_Buffer_Config");
         when 16#7F# => Log.Put_String ("Fabric_Commands");
         when 16#80# => Log.Put_String ("Format_NVM");
         when 16#81# => Log.Put_String ("Security_Send");
         when 16#82# => Log.Put_String ("Security_Receive");
         when 16#84# => Log.Put_String ("Sanitize");
         when 16#86# => Log.Put_String ("Get_LBA_Status");
         when others => Log.Put_String ("Unknown_Command");
      end case;
   end Put_NVMe_AdminCMD_Image;

   -------------------------------------------------------------------------

   procedure Put_NVMe_IOCMD_Image (TypeID : Interfaces.Unsigned_8)
   is
   begin
      case TypeID is
         when 16#00# => Log.Put_String ("Flush");
         when 16#01# => Log.Put_String ("Write");
         when 16#02# => Log.Put_String ("Read");
         when 16#04# => Log.Put_String ("Write_Uncorrectable");
         when 16#05# => Log.Put_String ("Compare");
         when 16#08# => Log.Put_String ("Write_Zeroes");
         when 16#09# => Log.Put_String ("Dataset_Mngmt");
         when 16#0C# => Log.Put_String ("Verify");
         when 16#0D# => Log.Put_String ("Reservation_Register");
         when 16#0E# => Log.Put_String ("Reservation_Report");
         when 16#11# => Log.Put_String ("Reservation_Acquire");
         when 16#15# => Log.Put_String ("Reservation_Release");
         when 16#19# => Log.Put_String ("Copy");
         when others => Log.Put_String ("Unknown_Command");
      end case;
   end Put_NVMe_IOCMD_Image;

   -------------------------------------------------------------------------

   Max_Status_Length : constant := 96;

   subtype Status_String is String (1 .. Max_Status_Length);

   type Status_Code_Array is array (Interfaces.Unsigned_8 range <>) of Status_String;

   -- Package-level constant arrays (stored in ROM/data section)
   Generic_Status_Codes : constant Status_Code_Array (0 ..  255) := (
      0          => "Generic - Successful Completion                                                                 ",
      1          => "Generic - Invalid Command Opcode                                                                ",
      2          => "Generic - Invalid Field in Command                                                              ",
      3          => "Generic - Command ID Conflict                                                                   ",
      4          => "Generic - Data Transfer Error                                                                   ",
      5          => "Generic - Commands Aborted due to Power Loss Notification                                       ",
      6          => "Generic - Internal Error                                                                        ",
      7          => "Generic - Command Abort Requested                                                               ",
      8          => "Generic - Command Aborted due to SQ Deletion                                                    ",
      9          => "Generic - Command Aborted due to Failed Fused Command                                           ",
      10         => "Generic - Command Aborted due to Missing Fused Command                                          ",
      11         => "Generic - Invalid Namespace or Format                                                           ",
      12         => "Generic - Command Sequence Error                                                                ",
      13         => "Generic - Invalid SGL Segment Descriptor                                                        ",
      14         => "Generic - Invalid Number of SGL Descriptors                                                     ",
      15         => "Generic - Data SGL Length Invalid                                                               ",
      16         => "Generic - Metadata SGL Length Invalid                                                           ",
      17         => "Generic - SGL Descriptor Type Invalid                                                           ",
      18         => "Generic - Invalid Use of Controller Memory Buffer                                               ",
      19         => "Generic - PRP Offset Invalid                                                                    ",
      20         => "Generic - Atomic Write Unit Exceeded (NVM, ZNS)                                                 ",
      21         => "Generic - Operation Denied                                                                      ",
      22         => "Generic - SGL Offset Invalid                                                                    ",
      23         => "Generic - RESERVED                                                                              ",
      24         => "Generic - Host Identifier Inconsistent Format                                                   ",
      25         => "Generic - Keep Alive Timer Expired                                                              ",
      26         => "Generic - Keep Alive Timeout Invalid                                                            ",
      27         => "Generic - Command Aborted due to Preempt and Abort                                              ",
      28         => "Generic - Sanitize Failed                                                                       ",
      29         => "Generic - Sanitize In Progress                                                                  ",
      30         => "Generic - SGL Data Block Granularity Invalid (NVM, ZNS)                                         ",
      31         => "Generic - Command Not Supported for Queue in CMB                                                ",
      32         => "Generic - Namespace is Write Protected                                                          ",
      33         => "Generic - Command Interrupted                                                                   ",
      34         => "Generic - Transient Transport Error                                                             ",
      35         => "Generic - Command Prohibited by Command and Feature Lockdown                                    ",
      36         => "Generic - Admin Command Media Not Ready                                                         ",
      37 .. 127  => "Generic - RESERVED                                                                              ",
      128        => "Generic - LBA Out of Range (NVM, ZNS)                                                           ",
      129        => "Generic - Capacity Exceeded                                                                     ",
      130        => "Generic - Namespace Not Ready                                                                   ",
      131        => "Generic - Reservation Conflict                                                                  ",
      132        => "Generic - Format In Progress (NVM, ZNS)                                                         ",
      133        => "Generic - Invalid Value Size                                                                    ",
      134        => "Generic - Invalid Key Size                                                                      ",
      135        => "Generic - KV Key Does Not Exist                                                                 ",
      136        => "Generic - Unrecovered Error                                                                     ",
      137        => "Generic - Key Exists                                                                            ",
      138 .. 191 => "Generic - RESERVED                                                                              ",
      192 .. 255 => "Generic - Vendor Specific                                                                       "
   );

   Command_Specific_Status_Codes : constant Status_Code_Array (0 .. 255) := (
      0          => "Command Specific - Completion Queue Invalid                                                     ",
      1          => "Command Specific - Invalid Queue Identifier                                                     ",
      2          => "Command Specific - Invalid Queue Size                                                           ",
      3          => "Command Specific - Abort Command Limit Exceeded                                                 ",
      4          => "Command Specific - RESERVED                                                                     ",
      5          => "Command Specific - Asynchronous Event Request Limit Exceeded                                    ",
      6          => "Command Specific - Invalid Firmware Slot                                                        ",
      7          => "Command Specific - Invalid Firmware Image                                                       ",
      8          => "Command Specific - Invalid Interrupt Vector                                                     ",
      9          => "Command Specific - Invalid Log Page                                                             ",
      10         => "Command Specific - Invalid Format                                                               ",
      11         => "Command Specific - Firmware Activation Requires Conventional Reset                              ",
      12         => "Command Specific - Invalid Queue Deletion                                                       ",
      13         => "Command Specific - Feature Identifier Not Saveable                                              ",
      14         => "Command Specific - Feature Not Changeable                                                       ",
      15         => "Command Specific - Feature Not Namespace Specific                                               ",
      16         => "Command Specific - Firmware Activation Requires NVM Subsystem Reset                             ",
      17         => "Command Specific - Firmware Activation Requires Controller Level Reset                          ",
      18         => "Command Specific - Firmware Activation Requires Maximum Time Violation                          ",
      19         => "Command Specific - Firmware Activation Prohibited                                               ",
      20         => "Command Specific - Overlapping Range                                                            ",
      21         => "Command Specific - Namespace Insufficient Capacity                                              ",
      22         => "Command Specific - Namespace Identifier Unavailable                                             ",
      23         => "Command Specific - RESERVED                                                                     ",
      24         => "Command Specific - Namespace Already Attached                                                   ",
      25         => "Command Specific - Namespace Is Private                                                         ",
      26         => "Command Specific - Namespace Not Attached                                                       ",
      27         => "Command Specific - Controller List Invalid                                                      ",
      28         => "Command Specific - Device Self-test In Progress                                                 ",
      29         => "Command Specific - Boot Partition Write Prohibited                                              ",
      30         => "Command Specific - Invalid Controller Identifier                                                ",
      31         => "Command Specific - Invalid Secondary Controller State                                           ",
      32         => "Command Specific - Invalid Number of Controller Resources                                       ",
      33         => "Command Specific - Invalid Resource Identifier                                                  ",
      34         => "Command Specific - Sanitize Prohibited While Persistent Memory Region is Enabled                ",
      35         => "Command Specific - ANA Group Identifier Invalid                                                 ",
      36         => "Command Specific - ANA Attach Failed                                                            ",
      37         => "Command Specific - Insufficient Capacity                                                        ",
      38         => "Command Specific - ANA Attach Failed                                                            ",
      39         => "Command Specific - Namespace Attachment Limit Exceeded                                          ",
      40         => "Command Specific - Prohibition of Command Execution Not Supported                               ",
      41         => "Command Specific - I/O Command Set Not Supported                                                ",
      42         => "Command Specific - I/O Command Set Not Enabled                                                  ",
      43         => "Command Specific - I/O Command Set Combination Rejected                                         ",
      44         => "Command Specific - Invalid I/O Command Set                                                      ",
      45         => "Command Specific - Identifier Unavailable                                                       ",
      46 ..  111 => "Command Specific - RESERVED                                                                     ",
      112 .. 127 => "Command Specific - Directive Specific                                                           ",
      128        => "Command Specific - I/O Command Set - Conflicting Attributes                                     ",
      129        => "Command Specific - I/O Command Set - Invalid Protection Information                             ",
      130        => "Command Specific - I/O Command Set - Attempted Write to Read Only Range                         ",
      131        => "Command Specific - I/O Command Set - Command Size Limit Exceeded                                ",
      132 .. 183 => "Command Specific - RESERVED                                                                     ",
      184        => "Command Specific - I/O Command Set - Zoned Boundary Error                                       ",
      185        => "Command Specific - I/O Command Set - Zone Is Full                                               ",
      186        => "Command Specific - I/O Command Set - Zone is Read Only                                          ",
      187        => "Command Specific - I/O Command Set - Zone is Offline                                            ",
      188        => "Command Specific - I/O Command Set - Zone is Invalid Write                                      ",
      189        => "Command Specific - I/O Command Set - Too Many Active Zones                                      ",
      190        => "Command Specific - I/O Command Set - Too Many Open Zones                                        ",
      191        => "Command Specific - I/O Command Set - Invalid Zone State Transition                              ",
      192 .. 255 => "Command Specific - Vendor Specific                                                              "
   );

   Media_Status_Codes : constant Status_Code_Array (0 .. 255) := (
      0   .. 127 => "Media and Data Integrity Errors - RESERVED                                                      ",
      128        => "Media and Data Integrity Errors - Write Fault                                                   ",
      129        => "Media and Data Integrity Errors - Unrecoverd Read Error                                         ",
      130        => "Media and Data Integrity Errors - End-to-end Guard Check Error                                  ",
      131        => "Media and Data Integrity Errors - End-to-end Application Tag Check Error                        ",
      132        => "Media and Data Integrity Errors - End-to-end Reference Tag Check Error                          ",
      133        => "Media and Data Integrity Errors - Compare Failure (NVM)                                         ",
      134        => "Media and Data Integrity Errors - Access Denied                                                 ",
      135        => "Media and Data Integrity Errors - Deallocated or Unwritten Logical Block (NVM)                  ",
      136        => "Media and Data Integrity Errors - End-to-End Storage Tag Check Error                            ",
      137 .. 191 => "Media and Data Integrity Errors - RESERVED                                                      ",
      192 .. 255 => "Media and Data Integrity Errors - Vendor Specific                                               "
   );

   Path_Status_Codes : constant Status_Code_Array (0 .. 255) := (
      0          => "Path Related Status - Internal Path Error                                                       ",
      1          => "Path Related Status - Asymmetric Access Persistent Loss                                         ",
      2          => "Path Related Status - Asymmetric Access Inaccessible                                            ",
      3          => "Path Related Status - Asymmetric Access Transition                                              ",
      4 ..   95  => "Path Related Status - RESERVED                                                                  ",
      96         => "Path Related Status - Controller Pathing Error - A pathing error was detected by the controller.",
      97 ..  111 => "Path Related Status - RESERVED                                                                  ",
      112        => "Path Related Status - Host Pathing Error - A pathing error was detected by the host.            ",
      113        => "Path Related Status - Command Aborted By Host                                                   ",
      114 .. 127 => "Path Related Status - RESERVED                                                                  ",
      128 .. 191 => "Path Related Status - Other Pathing error - I/O Command Set Specific                            ",
      192 .. 255 => "Path Related Status - Vendor Specific                                                           "
   );

   Reserved_Status_Codes : constant Status_Code_Array (0 .. 255) := (
      others => "RESERVED                                                                                        "
   );

   Vendor_Status_Codes : constant Status_Code_Array (0 .. 255) := (
      others => "Vendor Specific                                                                                 "
   );

   procedure Print_Status_Code (SC :  Interfaces.Unsigned_8; SCT : Unsigned_3) is
      function Msg_Length (S : Status_String) return Natural
      with Post => Msg_Length'Result <= Max_Status_Length
      is
      begin
         for I in reverse S'Range loop
            if S (I) /= ' ' then
               return I;
            end if;
         end loop;
         return 0;
      end Msg_Length;

      Msg : Status_String;
   begin
      Log.Put_String ("Status Code: ");

      case SCT is
         when 0      => Msg := Generic_Status_Codes (SC);
         when 1      => Msg := Command_Specific_Status_Codes (SC);
         when 2      => Msg := Media_Status_Codes (SC);
         when 3      => Msg := Path_Status_Codes (SC);
         when 4 .. 6 => Msg := Reserved_Status_Codes (SC);
         when 7      => Msg := Vendor_Status_Codes (SC);
      end case;

      Log.Put_String (Msg (1 .. Msg_Length (Msg)));
      Log.New_Line;
   end Print_Status_Code;

end NVMe_Log;