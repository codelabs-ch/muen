--
--  Copyright (C) 2020 secunet Security Networks AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Debug_Ops;
with SK.Hypercall;
with SK.Strings;
with Interfaces;
with System;

with Muenblock;
with Muenblock.Request_Channel;
with Muenblock.Response_Channel;
with Muenblock.Request_Channel.Reader;
with Muenblock.Response_Channel.Writer_Instance;

with Ahci.Device;
with Mbr;

with Ports_Config;

with Ahci_Drv_Component.Memory_Arrays;
with Ahci_Drv_Component.Channel_Arrays;

use type Interfaces.Unsigned_32;
use type Interfaces.Unsigned_64;

package body Server
is
   package PC       renames Ports_Config;
   package MB       renames Muenblock;
   package Req_Chn  renames Muenblock.Request_Channel;
   package Resp_Chn renames Muenblock.Response_Channel;

   package CSpecs renames Ahci_Drv_Component.Channel_Arrays;

   Request_Channels_Size : constant := CSpecs.Blockdev_Request_Element_Count
     * CSpecs.Blockdev_Request_Element_Size * 8;

   type Request_Channel_Array is
     array (PC.Channel_Range) of Req_Chn.Channel_Type
     with
       Object_Size => Request_Channels_Size;

   type Request_Reader_Array
   is array (PC.Channel_Range)
      of Req_Chn.Reader.Reader_Type;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Request_Channels : Request_Channel_Array
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address (CSpecs.Blockdev_Request_Address_Base),
      Size    => Request_Channels_Size;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   Request_Readers : Request_Reader_Array
      := (others => Req_Chn.Reader.Null_Reader);

   Response_Channels_Size : constant := CSpecs.Blockdev_Response_Element_Count
     * CSpecs.Blockdev_Response_Element_Size * 8;

   type Response_Chan_Array is
     array (PC.Channel_Range) of Resp_Chn.Channel_Type
     with
       Object_Size => Response_Channels_Size;

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
   Response_Channels : Response_Chan_Array
   with
      Volatile,
      Async_Readers,
      Address => System'To_Address (CSpecs.Blockdev_Response_Address_Base),
      Size    => Response_Channels_Size;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   --  Combine requests from the client to maximize the request length to the
   --  device. We need to store the request tags to answer the requests after
   --  data transfer finished.

   --  Maximum number of request we combine to a single request
   Requ_Max : constant := 64;
   subtype Tag_Array_Range is Interfaces.Unsigned_32 range 0 .. Requ_Max;
   type Tag_Array_Type is array (Tag_Array_Range) of Interfaces.Unsigned_32;

   type Current_Request_Type is record
      Request_Kind   : MB.Request_Kind_Type;
      Device_Id      : Interfaces.Unsigned_16;
      Device_Offset  : Interfaces.Unsigned_64;
      Buffer_Offset  : Interfaces.Unsigned_64;
      Request_Length : Interfaces.Unsigned_64;
      Tags           : Tag_Array_Type;
      Tag_Idx        : Tag_Array_Range;
   end record;

   Null_Current : constant Current_Request_Type :=
      (Request_Kind   => MB.None,
       Device_Id      => 0,
       Device_Offset  => 0,
       Buffer_Offset  => 0,
       Request_Length => 0,
       Tag_Idx        => 0,
       Tags           => (others => 0));

   type Internal_Device_Type is record
      Ahci_Port     : Ahci.Port_Range;
      Partition     : Integer;
      --  Sector offset used for partition addressing
      Is_Valid      : Boolean;
      Sector_Offset : Interfaces.Unsigned_64;
      Sector_Count  : Interfaces.Unsigned_64;
      Current       : Current_Request_Type;
   end record;

   type Internal_Device_Array_Type is array (PC.Devices_Range)
      of Internal_Device_Type;

   type Port_Type is record
      Chan_Idx : PC.Channel_Range;
      Devs     : Internal_Device_Array_Type;
   end record;

   type Ports_Array is array (PC.Ports_Array_Range) of Port_Type;

   Ports : Ports_Array := (others =>
    (Chan_Idx         => 0,
     Devs             => (others => Internal_Device_Type'(
        Ahci_Port     => 0,
        Partition     => PC.Null_Partition,
        Sector_Offset => 0,
        Sector_Count  => 0,
        Is_Valid      => False,
        Current       => Null_Current))));

   --------------------------------------------------------------------

   procedure Init
   is
      use type Ahci.Port_Range;
      use type PC.Device;

      ID             : Ahci.Port_Range := 0;
      Mbr_Not_Read   : Boolean         := True;
      Port           : PC.Port_Config_Type;
      Dev            : PC.Device;
      Devs           : Ahci.Bit_Array (0 .. Integer (Ahci.Port_Range'Last));
      Mbr_Partitions : Mbr.Partition_Table_Type
        := Mbr.Null_Partition_Table;
   begin
      --  Init all attached devices, setup memory regions,...
      Ahci.Device.Init;
      Ahci.Device.Get_Attached_Devices (Devs);

      --  Check if the configured device / partition is available
      --  and setup partition offsets.
      for Port_Idx in PC.Port_Config'Range loop
         Port := PC.Port_Config (Port_Idx);
         Ports (Port_Idx).Chan_Idx := Port.Chan_Idx;
         for Dev_Idx in Port.Devices'Range loop
            Dev := Port.Devices (Dev_Idx);
            if Dev = PC.Null_Device then
               goto Next_Dev;
            end if;
            if not Devs (Integer (Dev.Ahci_Port)) then
               pragma Debug (Debug_Ops.Put_Line
                  ("WARNING: configured Device not available!"));
               goto Next_Dev;
            end if;
            --  parse the mbr if the device changed or we have not parsed it
            if ID /= Dev.Ahci_Port or else Mbr_Not_Read
            then
               ID := Dev.Ahci_Port;
               --  MBR?
               Mbr.Parse (ID, Mbr_Partitions);
               pragma Debug (Debug_Ops.Put_Line ("Partitions of Device " &
                  SK.Strings.Img (Interfaces.Unsigned_8 (ID))));
               pragma Debug (Debug_Ops.Print_MBR_Partition_Table (
                  Mbr_Partitions));
               Mbr_Not_Read := False;
            end if;

            --  is the partition available?
            if Mbr_Partitions.Count > Dev.Partition
               or else Dev.Partition = PC.No_Partition
            then
               Ports (Port_Idx).Devs (Dev_Idx).Is_Valid := True;
               Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port := Dev.Ahci_Port;
               if Dev.Partition = PC.No_Partition
               then
                  --  whole disk exported
                  Ports (Port_Idx).Devs (Dev_Idx).Sector_Offset := 0;
                  Ports (Port_Idx).Devs (Dev_Idx).Sector_Count
                     := Ahci.Device.Get_Sector_Cnt
                           (Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port);
               else
                  --  partition exported
                  Ports (Port_Idx).Devs (Dev_Idx).Sector_Offset
                     := Mbr_Partitions.Entries
                           (Dev.Partition).Start_Lba;
                  Ports (Port_Idx).Devs (Dev_Idx).Sector_Count
                     := Mbr_Partitions.Entries (Dev.Partition).Sector_Cnt;
               end if;
            end if;
            if Dev.Partition = PC.Smart_Only then
               Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port := Dev.Ahci_Port;
            end if;
            <<Next_Dev>>
         end loop;
      end loop;

      --  Initialize all writer channels
      --
      --  TODO: Somehow this is not recognized by SPARK as write:
      --
      --  medium: "server.response_channels" might not be written after
      --  elaboration of main program "Ahci_Drv"[#0].
      --
      --  This message goes away if Response_Channels is annotated with
      --  Async_Writers. Might be a SPARK bug.

      for Chn of Response_Channels loop
         Resp_Chn.Writer_Instance.Initialize
            (Channel => Chn,
             Epoch   => 1);
      end loop;

   end Init;

   --------------------------------------------------------------------

   procedure Send_Response
      (Chan_Idx : PC.Channel_Range;
       Response : MB.Block_Response_Type)
   is
   begin
      Resp_Chn.Writer_Instance.Write
        (Channel => Response_Channels (Chan_Idx),
         Element => Response);
      SK.Hypercall.Trigger_Event (
         Number => Interfaces.Unsigned_8 (
            Ahci_Drv_Component.Channel_Arrays.Blockdev_Response_Event_Base
                     + Integer (Chan_Idx)));
   end Send_Response;

   --------------------------------------------------------------------

   function Get_Shm_Buffer_Base (Shm_Idx : PC.Channel_Range)
      return Interfaces.Unsigned_64
   is
      package A renames Ahci_Drv_Component.Memory_Arrays;
   begin
      return A.Blockdev_Shm_Address_Base +
         Interfaces.Unsigned_64 (Shm_Idx) * A.Blockdev_Shm_Element_Size;
   end Get_Shm_Buffer_Base;

   --------------------------------------------------------------------

   procedure Finish_Current_Request
      (Port_Idx : PC.Ports_Array_Range;
       Dev_Idx  : PC.Devices_Range;
       SendResp : Boolean := True)
   with
      Pre  => Musinfo.Instance.Is_Valid,
      Post => Ports (Port_Idx).Devs (Dev_Idx).Current = Null_Current
   is
      use type Ahci.Status_Type;

      Ret         : Ahci.Status_Type := Ahci.EIO;
      Dev_Id      : constant Ahci.Port_Range
                     := Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port;
      Sector_Size : constant Interfaces.Unsigned_32
                     := Ahci.Device.Get_Sector_Size (Dev_Id);
      Start_Sec   : constant Interfaces.Unsigned_64
                     := Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Offset +
                        Ports (Port_Idx).Devs (Dev_Idx).Sector_Offset;
      Sec_Cnt     : Interfaces.Unsigned_32;
      Response    : MB.Block_Response_Type;
      Address     : constant Interfaces.Unsigned_64
                     := Ports (Port_Idx).Devs (Dev_Idx).Current.Buffer_Offset +
                              Get_Shm_Buffer_Base (Ports (Port_Idx).Chan_Idx);
   begin
      if Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx
        = Tag_Array_Range'First
        or else Sector_Size = 0
      then
         Ports (Port_Idx).Devs (Dev_Idx).Current := Null_Current;

         pragma Debug
           (Sector_Size = 0, Debug_Ops.Put_Line
              ("Finish_Current_Request with zero sector size for device "
               & "with ID " & SK.Strings.Img (Interfaces.Unsigned_64
                 (Dev_Id))));
         return;
      end if;

      Sec_Cnt := Interfaces.Unsigned_32'Mod
        (Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length /
             Interfaces.Unsigned_64 (Sector_Size));

      if ((Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Offset
               and Interfaces.Unsigned_64 ((Sector_Size - 1))) = 0)
      or ((Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length
               and Interfaces.Unsigned_64 ((Sector_Size - 1))) = 0)
      then
         case Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Kind is
            when MB.Read =>
               Ahci.Device.RW_Sectors
                  (ID      => Dev_Id,
                   RW      => Ahci.Read,
                   Start   => Start_Sec,
                   Count   => Sec_Cnt,
                   Address => Address,
                   Ret_Val => Ret);
            when MB.Write =>
               Ahci.Device.RW_Sectors
                  (ID      => Dev_Id,
                   RW      => Ahci.Write,
                   Start   => Start_Sec,
                   Count   => Sec_Cnt,
                   Address => Address,
                   Ret_Val => Ret);
            when MB.Discard =>
               Ahci.Device.Discard_Sectors
                  (ID      => Dev_Id,
                   Start   => Start_Sec,
                   Count   => Sec_Cnt,
                   Ret_Val => Ret);
            when others =>
               Ports (Port_Idx).Devs (Dev_Idx).Current := Null_Current;
               return;
         end case;
      else
         pragma Debug (Debug_Ops.Put_Line
            ("Device Offset not aligned to Sector Size!" &
             SK.Strings.Img
                (Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Offset)));
      end if;

      pragma Debug (Ret /= Ahci.OK,
         Debug_Ops.Put_Line ("RW failed: Sector: " &
            SK.Strings.Img (Start_Sec) &
            " Number of Sectors: " &
            SK.Strings.Img (Sec_Cnt) &
            " Ret: " &
            SK.Strings.Img (Ahci.Status_To_Unsigned64 (Ret))));

      Response.Request_Kind
         := Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Kind;
      Response.Device_Id
         := Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Id;
      Response.Status_Code
         := Ahci.Status_To_Unsigned64 (Ret);

      for I in Tag_Array_Range range
        Tag_Array_Range'First ..
          Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx - 1
      loop
         Response.Request_Tag
            := Ports (Port_Idx).Devs (Dev_Idx).Current.Tags (I);
         if SendResp then
            Send_Response (Ports (Port_Idx).Chan_Idx, Response);
         end if;
      end loop;

      Ports (Port_Idx).Devs (Dev_Idx).Current := Null_Current;
   end Finish_Current_Request;

   --------------------------------------------------------------------

   procedure Process_Simple_Request
      (Port_Idx : PC.Ports_Array_Range;
       Dev_Idx  : PC.Devices_Range;
       Request  : MB.Block_Request_Type)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
      use type Ahci.Status_Type;

      Ret      : Ahci.Status_Type;
      Response : MB.Block_Response_Type;
   begin
      Response.Request_Kind := Request.Request_Kind;
      Response.Device_Id    := Request.Device_Id;
      Response.Request_Tag  := Request.Request_Tag;
      Response.Status_Code  := 0;

      case Request.Request_Kind is
         when MB.Media_Blocks =>
            Response.Status_Code :=
               Ports (Port_Idx).Devs (Dev_Idx).Sector_Count;
         when MB.Block_Length =>
            Response.Status_Code :=
               Interfaces.Unsigned_64
                  (Ahci.Device.Get_Sector_Size
                     (Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port));
         when MB.Max_Devices =>
            declare
               Cnt : Interfaces.Unsigned_64 := 0;
            begin
               for Dev of Ports (Port_Idx).Devs loop
                  if Dev.Is_Valid then
                     Cnt := Cnt + 1;
                  end if;
               end loop;
               Response.Status_Code := Cnt;
            end;
         when MB.Max_Blocks_Count =>
            Response.Status_Code :=
               Interfaces.Unsigned_64
                  (Ahci.Device.Get_Max_Sector_Count
                     (Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port));
         when MB.Reset =>
            for I in Ports (Port_Idx).Devs'Range loop
               if Ports (Port_Idx).Devs (I).Is_Valid then
                  Finish_Current_Request (Port_Idx, I, False);
               end if;
            end loop;
            Response.Status_Code := 0;
         when MB.Get_SMART =>
            declare
               Address      : constant Interfaces.Unsigned_64
                   := Ports (Port_Idx).Devs (Dev_Idx).Current.Buffer_Offset +
                            Get_Shm_Buffer_Base (Ports (Port_Idx).Chan_Idx);
               SMART_Status : Ahci.Device.SMART_Status_Type;
            begin
               Ahci.Device.Get_SMART
                 (ID      => Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port,
                  Address => Address,
                  Status  => SMART_Status,
                  Ret_Val => Ret);
               if Ret  = Ahci.OK then
                  case SMART_Status is
                     when Ahci.Device.OK =>
                        Response.Status_Code := MB.SMART_OK;
                     when Ahci.Device.Threshold_Exceeded =>
                        Response.Status_Code := MB.SMART_THRESHOLD_EXCEEDED;
                     when Ahci.Device.Undefined =>
                        Response.Status_Code := MB.SMART_UNDEFINED;
                  end case;
               else
                  Response.Status_Code := 0;
               end if;
            end;
         when MB.Sync =>
            --  end all outstanding requests
            for I in Ports (Port_Idx).Devs'Range loop
               if Ports (Port_Idx).Devs (I).Is_Valid then
                  Finish_Current_Request (Port_Idx, I);
               end if;
            end loop;

            Ahci.Device.Sync
               (ID      => Ports (Port_Idx).Devs (Dev_Idx).Ahci_Port,
                Ret_Val => Ret);

            if Ret = Ahci.OK then
               Response.Status_Code := 0;
            else
               Response.Status_Code := 1;
            end if;

         when others =>
            pragma Debug (Debug_Ops.Put_Line ("simple_req: unknown!"));
            null;
      end case;

      Send_Response (Ports (Port_Idx).Chan_Idx, Response);
   end Process_Simple_Request;

   --------------------------------------------------------------------

   procedure Process_RWD_Request
      (Port_Idx : PC.Ports_Array_Range;
       Dev_Idx  : PC.Devices_Range;
       Request  : MB.Block_Request_Type)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
      use type MB.Request_Kind_Type;
      use type Interfaces.Unsigned_16;

      Response : MB.Block_Response_Type;
   begin
      if not Ports (Port_Idx).Devs (Dev_Idx).Is_Valid then
         Response.Request_Kind := Request.Request_Kind;
         Response.Request_Tag  := Request.Request_Tag;
         Response.Device_Id    := Request.Device_Id;
         Response.Status_Code  := 1;
         Send_Response (Ports (Port_Idx).Chan_Idx, Response);
         return;
      end if;

      --  For performance reasons the server queues the requests instead
      --  of sending each single request to the device. For contiguous
      --  reads this has a noticeable performance impact.
      --  Therefore we must check for each new request if it's contiguous
      --  with the requests in-flight or if we have to finish the last
      --  requests first and start a new request.
      if Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx
          /= Tag_Array_Range'First
        and then
          ((Request.Request_Kind
            /= Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Kind)
         or (Request.Buffer_Offset
            /= Ports (Port_Idx).Devs (Dev_Idx).Current.Buffer_Offset +
               Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length)
         or (Request.Device_Offset
            /= Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Offset +
               Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length)
         or (Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx
            = Tag_Array_Range'Last)
         or (Request.Device_Id
            /= Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Id))
      then
         Finish_Current_Request (Port_Idx, Dev_Idx);
      end if;

      if Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Kind = MB.None
      then
         --  first request -> setup current fields
         Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Offset
            := Request.Device_Offset;
         Ports (Port_Idx).Devs (Dev_Idx).Current.Device_Id
            := Request.Device_Id;
         Ports (Port_Idx).Devs (Dev_Idx).Current.Buffer_Offset
            := Request.Buffer_Offset;
         Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Kind
            := Request.Request_Kind;
      end if;

      Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length
         := Ports (Port_Idx).Devs (Dev_Idx).Current.Request_Length +
            Request.Request_Length;
      Ports (Port_Idx).Devs (Dev_Idx).Current.Tags
         (Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx)
            := Request.Request_Tag;
      Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx
         := Ports (Port_Idx).Devs (Dev_Idx).Current.Tag_Idx + 1;
   end Process_RWD_Request;

   --------------------------------------------------------------------

   procedure Process_Request
      (Port_Idx : PC.Ports_Array_Range;
       Request  : MB.Block_Request_Type)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
      use type Interfaces.Unsigned_16;

      Dev_Idx : PC.Devices_Range;
   begin
      --  pragma Debug (Debug_Ops.Put_Line ("Received request on port: " &
      --       SK.Strings.Img (Interfaces.Unsigned_32 (Port_Idx))));
      --  pragma Debug (Debug_Ops.Print_Request (Request));

      if Request.Device_Id > Interfaces.Unsigned_16 (PC.Devices_Range'Last)
      then
         pragma Debug
           (Debug_Ops.Put_Line ("Request on port: " &
              SK.Strings.Img (Interfaces.Unsigned_32 (Port_Idx))
            & " -  invalid device ID, ignoring request"));

         declare
            Response : MB.Block_Response_Type;
         begin
            Response.Request_Kind := Request.Request_Kind;
            Response.Request_Tag  := Request.Request_Tag;
            Response.Device_Id    := Request.Device_Id;

            case Request.Request_Kind is
               when MB.None | MB.Read | MB.Write | MB.Discard | MB.Sync
                  | MB.Reset =>
                  Response.Status_Code := 1;
               when  others =>
                  Response.Status_Code := 0;
            end case;

            Send_Response
              (Chan_Idx => Ports (Port_Idx).Chan_Idx,
               Response => Response);
         end;
         return;
      end if;

      Dev_Idx := PC.Devices_Range (Request.Device_Id);

      case Request.Request_Kind is
         when MB.Read | MB.Write | MB.Discard =>
            Process_RWD_Request (Port_Idx, Dev_Idx, Request);
         when MB.Media_Blocks
               | MB.Block_Length
               | MB.Max_Blocks_Count
               | MB.Max_Devices
               | MB.Reset
               | MB.Get_SMART
               | MB.Sync =>
            Process_Simple_Request (Port_Idx, Dev_Idx, Request);
         when others =>
            pragma Debug (Debug_Ops.Put_Line ("unknown request!"));
            null;
      end case;

   end Process_Request;

   --------------------------------------------------------------------

   procedure Finish_Current_Requests
      (Port_Idx : PC.Ports_Array_Range)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
   begin
      for Dev_Idx in Ports (Port_Idx).Devs'Range loop
         if Ports (Port_Idx).Devs (Dev_Idx).Is_Valid then
            Finish_Current_Request (Port_Idx, Dev_Idx);
         end if;
      end loop;
   end Finish_Current_Requests;

   --------------------------------------------------------------------

   procedure Process_Port
      (Port_Idx : PC.Ports_Array_Range)
   with
      Pre => Musinfo.Instance.Is_Valid
   is
      use type Req_Chn.Reader.Result_Type;

      Request : MB.Block_Request_Type;
      Res     : Req_Chn.Reader.Result_Type;
   begin
      Process_Loop : loop
         Req_Chn.Reader.Read
               (Channel => Request_Channels (Ports (Port_Idx).Chan_Idx),
                Reader  => Request_Readers (Ports (Port_Idx).Chan_Idx),
                Element => Request,
                Result  => Res);
         case Res is
            when Req_Chn.Reader.Incompatible_Interface =>
               pragma Debug (Debug_Ops.Put_Line
                 (Item => "Request channel: Incompatible interface"
                  & " detected"));
            when Req_Chn.Reader.Epoch_Changed =>
               pragma Debug (Debug_Ops.Put_Line
                 (Item => "Request channel: Epoch changed"));
            when Req_Chn.Reader.No_Data =>
               Finish_Current_Requests (Port_Idx);
            when Req_Chn.Reader.Overrun_Detected =>
               pragma Debug (Debug_Ops.Put_Line
                 (Item => "Overrun!"));
            when Req_Chn.Reader.Inactive =>
               pragma Debug (Debug_Ops.Put_Line
                  (Item => "Request channel: Inactive"));
            when Req_Chn.Reader.Success =>
               Process_Request (Port_Idx, Request);
         end case;
         exit Process_Loop when Res /= Req_Chn.Reader.Success;
      end loop Process_Loop;
   end Process_Port;

   --------------------------------------------------------------------

   procedure Process
   is
      Active : Boolean;
   begin
      Process_Loop : loop
         for Port_Idx in Ports'Range loop
            Req_Chn.Is_Active
               (Request_Channels (Ports (Port_Idx).Chan_Idx), Active);
            if Active then
               Process_Port (Port_Idx);
            end if;
         end loop;
      end loop Process_Loop;
   end Process;

end Server;
