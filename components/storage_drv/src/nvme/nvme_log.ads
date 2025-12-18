with Storage_Interface; use Storage_Interface;

with Interfaces;

package NVMe_Log is

   pragma Warnings (GNATprove, Off, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");
   procedure Put_NVMe_AdminCMD_Image (TypeID : Interfaces.Unsigned_8);
   pragma Warnings (GNATprove, On, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");

   pragma Warnings (GNATprove, Off, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");
   procedure Put_NVMe_IOCMD_Image (TypeID : Interfaces.Unsigned_8);
   pragma Warnings (GNATprove, On, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");

   pragma Warnings (GNATprove, Off, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");
   procedure Print_Status_Code (SC : Interfaces.Unsigned_8; SCT : Unsigned_3);
   pragma Warnings (GNATprove, On, "subprogram * has no effect",
                    Reason => "SPARK does not know about Debuglog.Client.State");

end NVMe_Log;