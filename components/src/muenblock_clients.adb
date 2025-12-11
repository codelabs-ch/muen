--
--  Copyright (C) 2020 secunet Security Networks AG
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;
with SK.Hypercall;
with SK.CPU;

with Debuglog.Client;
with Musinfo.Instance;
with Muenblock.Request_Channel;
with Muenblock.Response_Channel;
with Muenblock.Request_Channel.Writer_Instance;
with Muenblock.Response_Channel.Reader;

package body Muenblock_Clients
is
   package MB       renames Muenblock;
   package Req_Chn  renames MB.Request_Channel;
   package Resp_Chn renames MB.Response_Channel;

   type Device_Count_Type is new Interfaces.Unsigned_16 range
     0 .. Devices_Cnt_Max;

   Request_Channel_Size : constant
     := (MB.Block_Request_Type_Size * MB.Request_Channel_Elements + 64) * 8;

   subtype Request_Channel_Type is Req_Chn.Channel_Type
     with Object_Size => Request_Channel_Size;

   type Request_Channels_Array_Type is array (Client_Range_Type)
     of Request_Channel_Type
   with Independent_Components;

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
   Request_Channels : Request_Channels_Array_Type
   with
      Volatile,
      Async_Readers,
      Address     => Req_Channel_Base_Address;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   Response_Channel_Size : constant
     := (MB.Block_Response_Size * MB.Response_Channel_Elements + 64) * 8;

   subtype Response_Channel_Type is Resp_Chn.Channel_Type
     with Object_Size => Response_Channel_Size;

   type Response_Channels_Array_Type is array (Client_Range_Type)
     of Response_Channel_Type
   with Independent_Components;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Response_Channels : Response_Channels_Array_Type
   with
      Volatile,
      Async_Writers,
      Address => Resp_Channel_Base_Address;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   type Readers_Array_Type is array (Client_Range_Type)
     of Resp_Chn.Reader.Reader_Type;

   Readers : Readers_Array_Type := (others => Resp_Chn.Reader.Null_Reader);

   Operations_Time_Out : Integer := Integer'Last;

   type Device_Info_Type is record
      Sector_Cnt  : Interfaces.Unsigned_64;
      Sector_Size : Interfaces.Unsigned_64;
      Max_Sectors : Interfaces.Unsigned_64;
      Valid       : Boolean;
   end record;

   Null_Device_Info : constant Device_Info_Type
     := (Sector_Cnt  => 0,
         Sector_Size => 0,
         Max_Sectors => 0,
         Valid       => False);

   type Device_Info_Array_Type is array (Device_Range_Type)
     of Device_Info_Type;

   type Devices_Info_Type is record
      Count : Device_Count_Type;
      Infos : Device_Info_Array_Type;
   end record;

   Null_Devices_Info_Type : constant Devices_Info_Type
     := (Count => Device_Count_Type'First,
         Infos => (others => Null_Device_Info));

   type Client_Device_Info_Array_Type is array (Client_Range_Type)
     of Devices_Info_Type;

   Device_Infos : Client_Device_Info_Array_Type
      := (others => Null_Devices_Info_Type);

   --  Receive data from specified Muenblock server.
   procedure Receive
     (Client   :     Client_Range_Type;
      Response : out MB.Block_Response_Type;
      Error    : out Boolean);

   -------------------------------------------------------------------------

   procedure Receive
     (Client       :     Client_Range_Type;
      Response : out MB.Block_Response_Type;
      Error    : out Boolean)
   with
      SPARK_Mode => Off
   is
      use type Interfaces.Unsigned_64;
      use type Resp_Chn.Reader.Result_Type;

      Res     : Resp_Chn.Reader.Result_Type;
      Now     : Interfaces.Unsigned_64;
      Timeout : constant Interfaces.Unsigned_64
        := Musinfo.Instance.TSC_Schedule_End
            + Musinfo.Instance.TSC_Khz
            * Interfaces.Unsigned_64 (Operations_Time_Out);
   begin
      --  wait for responses
      Now   := Musinfo.Instance.TSC_Schedule_Start;
      Error := False;

      Wait_Data :
      loop
         Resp_Chn.Reader.Read
           (Channel => Response_Channels (Client),
            Reader  => Readers (Client),
            Element => Response,
            Result  => Res);
         case Res is
            when Resp_Chn.Reader.Incompatible_Interface =>
               Debuglog.Client.Put_Line
                 (Item => "Response channel: Incompatible interface"
                  & " detected");
               Error := True;
            when Resp_Chn.Reader.Epoch_Changed =>
               Debuglog.Client.Put_Line
                 (Item => "Response channel: Epoch changed");
               Error := False;
            when Resp_Chn.Reader.No_Data =>
               SK.CPU.Hlt;
               Error := False;
            when Resp_Chn.Reader.Overrun_Detected =>
               Debuglog.Client.Put_Line
                 (Item => "Response channel: Overrun!");
               Error := True;
            when Resp_Chn.Reader.Inactive =>
               Debuglog.Client.Put_Line
                 (Item => "Response channel: Inactive");
               Error := True;
            when Resp_Chn.Reader.Success =>
               Error := False;
         end case;

         Now := Musinfo.Instance.TSC_Schedule_Start;
         if Now > Timeout then
            Debuglog.Client.Put_Line
                 (Item => "Response channel: Timeout");
            Error := True;
         end if;

         exit Wait_Data when Res = Resp_Chn.Reader.Success or Error;
      end loop Wait_Data;
   end Receive;

   -------------------------------------------------------------------------

   procedure Send_Request
     (Client  : Client_Range_Type;
      Request : MB.Block_Request_Type)
   is
      use type Interfaces.Unsigned_8;

      Event_Nr : constant Interfaces.Unsigned_8
        := Base_Event_Number + Interfaces.Unsigned_8
          (Client - Client_Range_Type'First);
   begin
      Req_Chn.Writer_Instance.Write
         (Channel => Request_Channels (Client),
          Element => Request);
      SK.Hypercall.Trigger_Event (Number => Event_Nr);
   end Send_Request;

   -------------------------------------------------------------------------

   --  Returns the number of devices on this channel pair
   procedure Get_Devices_Cnt
     (Client :     Client_Range_Type;
      Cnt    : out Device_Count_Type)
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;
      use type MB.Request_Kind_Type;

      Request  : MB.Block_Request_Type  := MB.Null_Request;
      Response : MB.Block_Response_Type;
      Error    : Boolean;
   begin
      Request.Request_Kind := MB.Max_Devices;
      Request.Request_Tag  := 1;
      Cnt := 0;

      Send_Request (Client, Request);
      Receive (Client, Response, Error);
      if not Error
         and (Response.Request_Kind = MB.Max_Devices)
         and (Response.Request_Tag = 1)
      then
         if Response.Status_Code <=
           Interfaces.Unsigned_64 (Device_Count_Type'Last)
         then
            Cnt := Device_Count_Type (Response.Status_Code);
         else
            Debuglog.Client.Put_Line
              (Item => "More devices than we can handle!");
            Cnt := Device_Count_Type'Last;
         end if;
      end if;
   end Get_Devices_Cnt;

   -------------------------------------------------------------------------

   procedure Get_Device_Info
     (Client      :     Client_Range_Type := Client_Range_Type'First;
      Device_Id   :     Device_Range_Type;
      Sector_Cnt  : out Interfaces.Unsigned_64;
      Sector_Size : out Interfaces.Unsigned_64;
      Max_Sectors : out Interfaces.Unsigned_64;
      Valid       : out Boolean)
   is
      use type Interfaces.Unsigned_64;

      Request  : MB.Block_Request_Type := MB.Null_Request;
      Response : MB.Block_Response_Type;
      Error    : Boolean;

      Device_Cnt  : Device_Count_Type renames Device_Infos (Client).Count;
      Device_Info : Device_Info_Type  renames Device_Infos (Client).Infos
        (Device_Id);
   begin
      Sector_Cnt  := Interfaces.Unsigned_64'Last;
      Sector_Size := Interfaces.Unsigned_64'Last;
      Max_Sectors := Interfaces.Unsigned_64'Last;
      Valid       := False;

      if (Device_Cnt = 0) or (Device_Count_Type (Device_Id) > (Device_Cnt - 1))
      then
         return;
      end if;

      if Device_Info.Valid then
         Sector_Cnt  := Device_Info.Sector_Cnt;
         Sector_Size := Device_Info.Sector_Size;
         Max_Sectors := Device_Info.Max_Sectors;
         Valid       := True;
         return;
      end if;

      Request.Request_Kind := Muenblock.Media_Blocks;
      Request.Request_Tag  := 10;
      Request.Device_Id    := Interfaces.Unsigned_16 (Device_Id);
      Send_Request (Client, Request);

      Request.Request_Kind := MB.Block_Length;
      Request.Request_Tag  := 11;
      Send_Request (Client, Request);

      Request.Request_Kind := MB.Max_Blocks_Count;
      Request.Request_Tag  := 12;
      Send_Request (Client, Request);

      while (Sector_Cnt  = Interfaces.Unsigned_64'Last)
         or (Sector_Size = Interfaces.Unsigned_64'Last)
         or (Max_Sectors = Interfaces.Unsigned_64'Last)
      loop
         Receive (Client, Response, Error);
         if not Error then
            case Response.Request_Tag is
               when 10     => Sector_Cnt  := Response.Status_Code;
               when 11     => Sector_Size := Response.Status_Code;
               when 12     => Max_Sectors := Response.Status_Code;
               when others => null;
            end case;
         end if;
      end loop;

      Valid := True;
   end Get_Device_Info;

   -------------------------------------------------------------------------

   procedure Get_SMART
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_32;
      use type MB.Request_Kind_Type;

      Request  : MB.Block_Request_Type  := MB.Null_Request;
      Response : MB.Block_Response_Type;
      Tag      : constant Interfaces.Unsigned_32 := 16#534d4152#;
      Error    : Boolean;
   begin
      Request.Request_Kind  := MB.Get_SMART;
      Request.Request_Tag   := Tag;
      Request.Device_Id     := Interfaces.Unsigned_16 (Device_Id);
      Request.Buffer_Offset := Buffer_Offset;

      Send_Request (Client, Request);

      Receive (Client, Response, Error);
      if not Error
         and (Response.Request_Kind = MB.Get_SMART)
         and (Response.Request_Tag = Tag)
      then
         Result := Response.Status_Code;
      else
         Result := 0;
      end if;
   end Get_SMART;

   -------------------------------------------------------------------------

   procedure Init
     (Client     : Client_Range_Type := Client_Range_Type'First;
      Timeout_MS : Integer := Integer'Last)
   with
      SPARK_Mode => Off
   is
      Active : Boolean;
   begin
      Req_Chn.Writer_Instance.Initialize
        (Channel => Request_Channels (Client),
         Epoch   => 1);
      Operations_Time_Out := Timeout_MS;

      Resp_Chn.Reader.Drain
           (Channel => Response_Channels (Client),
            Reader  => Readers (Client));

      --  wait for the server to become active
      Wait_Active :
      loop
         Resp_Chn.Is_Active
           (Channel => Response_Channels (Client),
            Result  => Active);
         exit Wait_Active when Active;
         pragma Loop_Invariant (not Active);
      end loop Wait_Active;

      declare
         use type MB.Request_Kind_Type;

         Request      : MB.Block_Request_Type := MB.Null_Request;
         Response     : MB.Block_Response_Type;
         Unused_Error : Boolean;
      begin
         Request.Request_Kind := MB.Reset;

         Send_Request (Client, Request);

         Receive (Client, Response, Unused_Error);
         if Response.Request_Kind = MB.Reset then
            Debuglog.Client.Put_Line (Item => "Server request done!");
         end if;
      end;

      --  Get Number of attached devices
      Get_Devices_Cnt (Client, Device_Infos (Client).Count);

      if Device_Infos (Client).Count = 0 then
         Debuglog.Client.Put_Line (Item => "No devices found!");
      end if;

      --  fill device Info
      declare
         Dev_Infos : Device_Info_Array_Type renames Device_Infos (Client).Infos;
      begin
         for I in Device_Range_Type loop
            Get_Device_Info
              (Client      => Client,
               Device_Id   => I,
               Sector_Cnt  => Dev_Infos (I).Sector_Cnt,
               Sector_Size => Dev_Infos (I).Sector_Size,
               Max_Sectors => Dev_Infos (I).Max_Sectors,
               Valid       => Dev_Infos (I).Valid);
         end loop;
      end;
   end Init;

   -------------------------------------------------------------------------

   procedure RW
     (Client        :     Client_Range_Type;
      Device_Id     :     Device_Range_Type;
      Request_Kind  :     MB.Request_Kind_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Cnt          : Interfaces.Unsigned_64 := Sector_Cnt;
      Now          : Interfaces.Unsigned_64;
      Error        : Boolean;
      Offset       : Interfaces.Unsigned_64 := 0;
      Request      : MB.Block_Request_Type;
      Requests_Cnt : Interfaces.Unsigned_32 := 0;
      Response     : MB.Block_Response_Type;

      Device_Cnt  : Device_Count_Type renames Device_Infos (Client).Count;
      Device_Info : Device_Info_Type  renames Device_Infos (Client).Infos
        (Device_Id);
   begin
      Result := 0;

      if (Device_Cnt = 0)
        or (Device_Count_Type (Device_Id) > (Device_Cnt - 1))
      then
         return;
      end if;

      Request.Request_Kind := Request_Kind;
      Request.Device_Id    := Interfaces.Unsigned_16 (Device_Id);

      while Cnt /= 0 loop
         if Cnt > Device_Info.Max_Sectors then
            Now := Device_Info.Max_Sectors;
         else
            Now := Cnt;
         end if;
         Request.Request_Tag    := 16#1000# + Requests_Cnt;
         Request.Buffer_Offset  := Buffer_Offset + Offset;
         Request.Device_Offset  := Start_Sector + Offset;
         Request.Request_Length := Now * Device_Info.Sector_Size;

         Send_Request (Client, Request);

         Cnt := Cnt - Now;
         Requests_Cnt := Requests_Cnt + 1;
         Offset := Offset + Now * Device_Info.Sector_Size;
      end loop;

      while Requests_Cnt /= 0 loop
         Receive (Client, Response, Error);
         Requests_Cnt := Requests_Cnt - 1;
         if Error or Response.Status_Code /= 0 then
            Debuglog.Client.Put_Line (Item => "Muenblock Request Failed!");
            Debuglog.Client.Put_Line (Item => " Client: "
               & SK.Strings.Img (Interfaces.Unsigned_16 (Client)));
            Debuglog.Client.Put_Line (Item => " Request: "
               & SK.Strings.Img (Interfaces.Unsigned_16
                  (Response.Request_Kind)));
            Debuglog.Client.Put_Line (Item => " Tag: "
               & SK.Strings.Img (Response.Request_Tag));
            Debuglog.Client.Put_Line (Item => " DeviceId: "
               & SK.Strings.Img (Interfaces.Unsigned_16 (Device_Id)));
            Debuglog.Client.Put_Line (Item => " Status: "
               & SK.Strings.Img (Response.Status_Code));
            return;
         end if;
      end loop;
      Result := Sector_Cnt;
   end RW;

   -------------------------------------------------------------------------

   procedure Discard
     (Client       :     Client_Range_Type := Client_Range_Type'First;
      Device_Id    :     Device_Range_Type;
      Start_Sector :     Interfaces.Unsigned_64;
      Sector_Cnt   :     Interfaces.Unsigned_64;
      Result       : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      RW (Client        => Client,
          Device_Id     => Device_Id,
          Request_Kind  => MB.Discard,
          Start_Sector  => Start_Sector,
          Buffer_Offset => 0,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
      Result := Result - Sector_Cnt;
   end Discard;

   -------------------------------------------------------------------------

   procedure Read
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64)
   is
   begin
      RW (Client        => Client,
          Device_Id     => Device_Id,
          Request_Kind  => MB.Read,
          Start_Sector  => Start_Sector,
          Buffer_Offset => Buffer_Offset,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
   end Read;

   -------------------------------------------------------------------------

   procedure Sync
     (Client    :     Client_Range_Type := Client_Range_Type'First;
      Device_Id :     Device_Range_Type;
      Result    : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_32;
      use type MB.Request_Kind_Type;

      Error    : Boolean;
      Request  : MB.Block_Request_Type;
      Response : MB.Block_Response_Type;

      Devices_Cnt : Device_Count_Type renames Device_Infos (Client).Count;
   begin
      Result := 1;

      if (Devices_Cnt = 0)
        or (Device_Count_Type (Device_Id) > (Devices_Cnt - 1))
      then
         return;
      end if;

      Request := (Request_Kind   => MB.Sync,
                  Device_Id      => Interfaces.Unsigned_16 (Device_Id),
                  Request_Tag    => 21,
                  Request_Length => 0,
                  Device_Offset  => 0,
                  Buffer_Offset  => 0);

      Send_Request (Client, Request);
      Receive (Client, Response, Error);

      if not Error
        and (Response.Request_Kind = MB.Sync)
        and (Response.Request_Tag = 21)
      then
         Result := Response.Status_Code;
      end if;
   end Sync;

   -------------------------------------------------------------------------

   procedure Write
     (Client        : Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64)
   is
   begin
      RW (Client        => Client,
          Device_Id     => Device_Id,
          Request_Kind  => MB.Write,
          Start_Sector  => Start_Sector,
          Buffer_Offset => Buffer_Offset,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
   end Write;

end Muenblock_Clients;
