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

with Debuglog.Client;
with Musinfo.Instance;
with SK.Strings;
with SK.Hypercall;
with SK.CPU;

with Muenblock.Request_Channel;
with Muenblock.Response_Channel;
with Muenblock.Request_Channel.Writer_Instance;
with Muenblock.Response_Channel.Reader;

with Interfaces;

use type Interfaces.Unsigned_32;
use type Interfaces.Unsigned_64;

package body Muenblock_Client
is
   package MB       renames Muenblock;
   package Req_Chn  renames MB.Request_Channel;
   package Resp_Chn renames MB.Response_Channel;

   Request_Channel : Req_Chn.Channel_Type
   with
      Volatile,
      Async_Readers,
      Address => Req_Channel_Address;

   Response_Channel : Resp_Chn.Channel_Type
   with
      Volatile,
      Async_Writers,
      Address => Resp_Channel_Address;

   Reader : Resp_Chn.Reader.Reader_Type
      := Resp_Chn.Reader.Null_Reader;

   Operations_Time_Out : Integer := Integer'Last;

   type Device_Info_Type is record
      Sector_Cnt  : Interfaces.Unsigned_64;
      Sector_Size : Interfaces.Unsigned_64;
      Max_Sectors : Interfaces.Unsigned_64;
      Valid       : Boolean;
   end record;

   type Device_Info_Array_Type is array (Device_Range_Type)
      of Device_Info_Type;

   Device_Info : Device_Info_Array_Type
      := (others => Device_Info_Type'(Sector_Cnt  => 0,
                                      Sector_Size => 0,
                                      Max_Sectors => 0,
                                      Valid       => False));

   --  Number of attached devices on the channel handled by this instance of
   --  MB client
   Devices_Cnt : Device_Range_Type := Device_Range_Type'First;

   --------------------------------------------------------------------

   procedure Receive
      (Response : out MB.Block_Response_Type;
       Error    : out Boolean)
   with
      SPARK_Mode => Off
   is
      use type Resp_Chn.Reader.Result_Type;

      Res     : Resp_Chn.Reader.Result_Type;
      Now     : Interfaces.Unsigned_64;
      Timeout : constant Interfaces.Unsigned_64
        := Musinfo.Instance.TSC_Schedule_End
            + Musinfo.Instance.TSC_Khz
            * Interfaces.Unsigned_64 (Operations_Time_Out);
   begin
      --  wait for responses
      Now := Musinfo.Instance.TSC_Schedule_Start;
      Error := False;

      Wait_Data : loop
         Resp_Chn.Reader.Read
           (Channel => Response_Channel,
            Reader  => Reader,
            Element => Response,
            Result  => Res);
         case Res is
            when Resp_Chn.Reader.Incompatible_Interface =>
               Debuglog.Client.Put_Line
                 (Item => "Request channel: Incompatible interface"
                  & " detected");
               Error := True;
            when Resp_Chn.Reader.Epoch_Changed =>
               Debuglog.Client.Put_Line
                 (Item => "Request channel: Epoch changed");
               Error := False;
            when Resp_Chn.Reader.No_Data =>
               SK.CPU.Hlt;
               Error := False;
            when Resp_Chn.Reader.Overrun_Detected =>
               Debuglog.Client.Put_Line
                 (Item => "Overrun!");
               Error := True;
            when Resp_Chn.Reader.Inactive =>
               Debuglog.Client.Put_Line
                  (Item => "Request channel: Inactive");
               Error := True;
            when Resp_Chn.Reader.Success =>
               Error := False;
         end case;

         Now := Musinfo.Instance.TSC_Schedule_Start;
         if Now > Timeout then
            Error := True;
         end if;

         exit Wait_Data when Res = Resp_Chn.Reader.Success or Error;
      end loop Wait_Data;
   end Receive;

   --------------------------------------------------------------------

   procedure Send_Request (Request : MB.Block_Request_Type)
   is
   begin
      Req_Chn.Writer_Instance.Write
         (Channel => Request_Channel,
          Element => Request);

      SK.Hypercall.Trigger_Event (Number => Event_Number);
   end Send_Request;

   --------------------------------------------------------------------

   --  Returns the number of devices on this channel pair
   procedure Get_Devices_Cnt (Cnt : out Device_Range_Type)
   is
      use type MB.Request_Kind_Type;

      Request  : MB.Block_Request_Type  := MB.Null_Request;
      Response : MB.Block_Response_Type := MB.Null_Response;
      Error    : Boolean;
   begin
      Request.Request_Kind := MB.Max_Devices;
      Request.Request_Tag  := 1;
      Cnt := 0;

      Send_Request (Request);
      Receive (Response, Error);
      if not Error
         and (Response.Request_Kind = MB.Max_Devices)
         and (Response.Request_Tag = 1)
      then
         if Response.Status_Code <=
               Interfaces.Unsigned_64 (Device_Range_Type'Last) + 1
         then
            Cnt := Device_Range_Type (Response.Status_Code);
         else
            Debuglog.Client.Put_Line
               (Item => "More devices than we can handle!");
            Cnt := Device_Range_Type'Last;
         end if;
      end if;
   end Get_Devices_Cnt;

   --------------------------------------------------------------------

   --  Get Number of Sectors and Sector Size of a given Device
   procedure Get_Device_Info
      (Device_Id   :     Device_Range_Type;
       Sector_Cnt  : out Interfaces.Unsigned_64;
       Sector_Size : out Interfaces.Unsigned_64;
       Max_Sectors : out Interfaces.Unsigned_64;
       Valid       : out Boolean)
   is
      Request  : MB.Block_Request_Type  := MB.Null_Request;
      Response : MB.Block_Response_Type := MB.Null_Response;
      Error    : Boolean;
   begin
      Sector_Cnt  := Interfaces.Unsigned_64'Last;
      Sector_Size := Interfaces.Unsigned_64'Last;
      Max_Sectors := Interfaces.Unsigned_64'Last;
      Valid       := False;

      if (Devices_Cnt = 0) or (Device_Id > (Devices_Cnt - 1)) then
         return;
      end if;

      if Device_Info (Device_Id).Valid then
         Sector_Cnt  := Device_Info (Device_Id).Sector_Cnt;
         Sector_Size := Device_Info (Device_Id).Sector_Size;
         Max_Sectors := Device_Info (Device_Id).Max_Sectors;
         Valid       := True;
         return;
      end if;

      Request.Request_Kind := MB.Media_Blocks;
      Request.Request_Tag  := 10;
      Request.Device_Id    := Interfaces.Unsigned_16 (Device_Id);
      Send_Request (Request);

      Request.Request_Kind := MB.Block_Length;
      Request.Request_Tag  := 11;
      Send_Request (Request);

      Request.Request_Kind := MB.Max_Blocks_Count;
      Request.Request_Tag  := 12;
      Send_Request (Request);

      while (Sector_Cnt  = Interfaces.Unsigned_64'Last)
         or (Sector_Size = Interfaces.Unsigned_64'Last)
         or (Max_Sectors = Interfaces.Unsigned_64'Last)
      loop
         Receive (Response, Error);
         if not Error
         then
            case Response.Request_Tag is
               when 10 =>
                  Sector_Cnt  := Response.Status_Code;
               when 11 =>
                  Sector_Size := Response.Status_Code;
               when 12 =>
                  Max_Sectors := Response.Status_Code;
               when others =>
                  null;
            end case;
         end if;
      end loop;

      Valid := True;
      Device_Info (Device_Id).Valid := True;
   end Get_Device_Info;

   --------------------------------------------------------------------

   procedure Get_SMART
      (Device_Id     :     Device_Range_Type;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64)
   is
      use type MB.Request_Kind_Type;

      Request  : MB.Block_Request_Type  := MB.Null_Request;
      Response : MB.Block_Response_Type := MB.Null_Response;
      Tag      : constant Interfaces.Unsigned_32 := 16#534d4152#;
      Error    : Boolean;
   begin
      Request.Request_Kind  := MB.Get_SMART;
      Request.Request_Tag   := Tag;
      Request.Device_Id     := Interfaces.Unsigned_16 (Device_Id);
      Request.Buffer_Offset := Buffer_Offset;

      Send_Request (Request);

      Receive (Response, Error);
      if not Error
         and (Response.Request_Kind = MB.Get_SMART)
         and (Response.Request_Tag = Tag)
      then
         Result := Response.Status_Code;
      else
         Result := 0;
      end if;
   end Get_SMART;

   --------------------------------------------------------------------

   procedure Init (Timeout_MS : Integer := Integer'Last)
   with
      SPARK_Mode => Off
   is
      Valid  : Boolean;
      Active : Boolean;
   begin
      Req_Chn.Writer_Instance.Initialize
        (Channel => Request_Channel,
         Epoch   => 1);
      Operations_Time_Out := Timeout_MS;

      Resp_Chn.Reader.Drain
           (Channel => Response_Channel,
            Reader  => Reader);

      --  wait for the server to become active
      Wait_Active : loop
         Resp_Chn.Is_Active (
            Channel => Response_Channel,
            Result => Active);
         exit Wait_Active when Active;
      end loop Wait_Active;

      declare
         use type MB.Request_Kind_Type;

         Request     : MB.Block_Request_Type
             := MB.Null_Request;
         Response    : MB.Block_Response_Type;
         Dummy_Error : Boolean;
      begin
         Request.Request_Kind := MB.Reset;

         Send_Request (Request);

         Receive (Response => Response,
                  Error    => Dummy_Error);
         if Response.Request_Kind = MB.Reset then
            Debuglog.Client.Put_Line (Item => "Server request done!");
         end if;
      end;

      --  Get Number of attached devices
      Get_Devices_Cnt (Devices_Cnt);

      if Devices_Cnt = 0 then
         Debuglog.Client.Put_Line (Item => "No devices found!");
      end if;

      --  fill device Info
      for I in Device_Range_Type loop
         Get_Device_Info (Device_Id   => I,
                          Sector_Cnt  => Device_Info (I).Sector_Cnt,
                          Sector_Size => Device_Info (I).Sector_Size,
                          Max_Sectors => Device_Info (I).Max_Sectors,
                          Valid       => Valid);
      end loop;
   end Init;

   --------------------------------------------------------------------

   procedure RW
      (Device_Id     :     Device_Range_Type;
       Request_Kind  :     MB.Request_Kind_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64)
   is
      Cnt          : Interfaces.Unsigned_64 := Sector_Cnt;
      Now          : Interfaces.Unsigned_64;
      Error        : Boolean;
      Offset       : Interfaces.Unsigned_64 := 0;
      Request      : MB.Block_Request_Type;
      Requests_Cnt : Interfaces.Unsigned_32 := 0;
      Response     : MB.Block_Response_Type := MB.Null_Response;
   begin
      Result := 0;

      if (Devices_Cnt = 0) or (Device_Id > (Devices_Cnt - 1)) then
         return;
      end if;

      Request.Request_Kind := Request_Kind;
      Request.Device_Id    := Interfaces.Unsigned_16 (Device_Id);

      while Cnt /= 0 loop
         if Cnt > Device_Info (Device_Id).Max_Sectors then
            Now := Device_Info (Device_Id).Max_Sectors;
         else
            Now := Cnt;
         end if;
         Request.Request_Tag    := 16#1000# + Requests_Cnt;
         Request.Buffer_Offset  := Buffer_Offset + Offset;
         Request.Device_Offset  := Start_Sector + Offset;
         Request.Request_Length := Now * Device_Info (Device_Id).Sector_Size;

         Send_Request (Request);

         Cnt := Cnt - Now;
         Requests_Cnt := Requests_Cnt + 1;
         Offset := Offset + Now * Device_Info (Device_Id).Sector_Size;
      end loop;

      while Requests_Cnt /= 0 loop
         Receive (Response, Error);
         Requests_Cnt := Requests_Cnt - 1;
         if Error or Response.Status_Code /= 0 then
            Debuglog.Client.Put_Line (Item => "Muenblock Request Failed!");
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

   --------------------------------------------------------------------

   procedure Discard
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64)
   is
   begin
      RW (Device_Id     => Device_Id,
          Request_Kind  => MB.Discard,
          Start_Sector  => Start_Sector,
          Buffer_Offset => 0,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
      Result := Result - Sector_Cnt;
   end Discard;

   --------------------------------------------------------------------

   procedure Read
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64)
   is
   begin
      RW (Device_Id     => Device_Id,
          Request_Kind  => MB.Read,
          Start_Sector  => Start_Sector,
          Buffer_Offset => Buffer_Offset,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
   end Read;

   --------------------------------------------------------------------

   procedure Sync
      (Device_Id :     Device_Range_Type;
       Result    : out Interfaces.Unsigned_64)
   is
      use type MB.Request_Kind_Type;

      Error    : Boolean;
      Request  : MB.Block_Request_Type;
      Response : MB.Block_Response_Type;
   begin
      Result := 1;

      if (Devices_Cnt = 0) or (Device_Id > (Devices_Cnt - 1)) then
         return;
      end if;

      Request.Request_Kind := MB.Sync;
      Request.Device_Id    := Interfaces.Unsigned_16 (Device_Id);
      Request.Request_Tag  := 21;

      Send_Request (Request);

      Receive (Response, Error);
      if not Error
         and (Response.Request_Kind = MB.Sync)
         and (Response.Request_Tag = 21)
      then
         Result := Response.Status_Code;
      end if;

   end Sync;

   --------------------------------------------------------------------

   procedure Write
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64)
   is
   begin
      RW (Device_Id     => Device_Id,
          Request_Kind  => MB.Write,
          Start_Sector  => Start_Sector,
          Buffer_Offset => Buffer_Offset,
          Sector_Cnt    => Sector_Cnt,
          Result        => Result);
   end Write;

end Muenblock_Client;
