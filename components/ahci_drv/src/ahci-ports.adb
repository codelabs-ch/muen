--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2020  secunet Security Networks AG
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

with Ahci.Delays;

with Debug_Ops;

with Interfaces;
use type Interfaces.Unsigned_16;
use type Interfaces.Unsigned_32;

package body Ahci.Ports
is

   Device_Not_Present  : constant Unsigned_4 := 16#0#;
   Present_Established : constant Unsigned_4 := 16#3#;

   Interface_Active    : constant Unsigned_4 := 16#1#;

   -------------------------------------------------------------------------

   procedure Clear_Errors
      (ID    : Port_Range;
       Clear : Clear_Error_Type)
   is
      Sata_Error  : Port_SATA_Error_Type;
      Intr_Status : Port_Interrupt_Status_Type;
   begin
      if Clear.Sata then
         Sata_Error.ERR  := 16#ffff#;
         Sata_Error.DIAG := 16#ffff#;
         Instance (ID).SATA_Error := Sata_Error;
      end if;
      if Clear.Intr then
         Intr_Status.DHRS := True;
         Intr_Status.PSS  := True;
         Intr_Status.DSS  := True;
         Intr_Status.SDBS := True;
         Intr_Status.DPS  := True;
         Intr_Status.DMPS := True;
         Intr_Status.IPMS := True;
         Intr_Status.OFS  := True;
         Intr_Status.INFS := True;
         Intr_Status.IFS  := True;
         Intr_Status.HBDS := True;
         Intr_Status.HBFS := True;
         Intr_Status.TFES := True;
         Intr_Status.CPDS := True;
         Instance (ID).Interrupt_Status := Intr_Status;
      end if;
   end Clear_Errors;
   -------------------------------------------------------------------------

   procedure Enable
     (ID      :     Port_Range;
      Success : out Boolean)
   is
      Status : Port_SATA_Status_Type;
      Cmd_St : Port_Command_Status_Type;
   begin
      Status := Instance (ID).SATA_Status;

      if Status.DET = Device_Not_Present then
         Success := False;
         return;
      end if;

      Cmd_St := Instance (ID).Command_And_Status;
      Cmd_St.ICC := Interface_Active;
      Instance (ID).Command_And_Status := Cmd_St;

      for I in Natural range 1 .. 10 loop
         Status := Instance (ID).SATA_Status;
         exit when Status.DET = Present_Established
           and then Status.IPM = Interface_Active;
         Delays.M_Delay (Msec => 1);
      end loop;

      Success := Status.DET = Present_Established
        and Status.IPM = Interface_Active;
   end Enable;

   -------------------------------------------------------------------------

   procedure Check_Error
     (ID    :     Port_Range;
      Error : out Boolean)
   is
      Sata_Error  : constant Port_SATA_Error_Type :=
                        Instance (ID).SATA_Error;
      Intr_Status : constant Port_Interrupt_Status_Type :=
                        Instance (ID).Interrupt_Status;
   begin
      Error := False;
      if Intr_Status.OFS
         or Intr_Status.INFS
         or Intr_Status.IFS
         or Intr_Status.HBDS
         or Intr_Status.HBFS
         or Intr_Status.TFES
         or Intr_Status.PCS
         or Sata_Error.ERR /= 0
         or Sata_Error.DIAG /= 0
      then
         Error := True;
         pragma Debug (Debug_Ops.Print_Port_Error (ID));
         pragma Debug (Debug_Ops.Dump_Port_Regs (ID));
      end if;
   end Check_Error;

   -------------------------------------------------------------------------

   procedure Recover_Errors (ID : Port_Range)
   is
      Tfd         : constant Port_Task_File_Data_Type :=
                        Instance (ID).Task_File_Data;
      Intr_Status : constant Port_Interrupt_Status_Type :=
                        Instance (ID).Interrupt_Status;
      Sata_Ctrl   : Port_SATA_Control_Type;
      Active      : Boolean;
      Clear       : Clear_Error_Type := (others => False);
   begin
      Instance (ID).Command_And_Status.ST := False;

      Clear.Sata := True;
      Clear_Errors (ID, Clear);
      if Tfd.STS.DRQ or Tfd.STS.BSY or Intr_Status.PCS then
         --  perform COMRESET
         Sata_Ctrl := Instance (ID).SATA_Control;
         Sata_Ctrl.DET := 1;
         Instance (ID).SATA_Control := Sata_Ctrl;
         Delays.M_Delay (1);
         Sata_Ctrl.DET := 0;
         Instance (ID).SATA_Control := Sata_Ctrl;
      end if;

      Is_Active (ID, Active);
      if Active then
         Start (ID);
      end if;
   end Recover_Errors;

   -------------------------------------------------------------------------

   procedure Execute
      (ID      :     Port_Range;
       Success : out Boolean)
   is
      Local_Cmd_Issue  : Bit_Array (0 .. 31);
      Local_Int_Status : Port_Interrupt_Status_Type;
      Local_Cmd_Status : Port_Command_Status_Type;
      Error            : Boolean;
      Clear            : Clear_Error_Type := (others => False);
      Now              : Interfaces.Unsigned_64;
      Timeout          : constant Interfaces.Unsigned_64
        := Musinfo.Instance.TSC_Schedule_End +
            5 * Musinfo.Instance.TSC_Khz * 1000;
   begin
      Local_Cmd_Status := Instance (ID).Command_And_Status;
      Local_Cmd_Issue := Instance (ID).Command_Issue;

      if Local_Cmd_Status.CR = False or Local_Cmd_Issue (0) then
         pragma Debug (Debug_Ops.Put_Line ("Busy.." &
            SK.Strings.Img (Interfaces.Unsigned_32 (ID))));
            Success := False;
            return;
      end if;

      --  trigger cmd execution
      Local_Cmd_Issue (0) := True;
      Instance (ID).Command_Issue := Local_Cmd_Issue;

      --  wait command execution (Up to 5s)
      for Time_Out in Natural range 1 .. 5_000 loop
         Local_Cmd_Issue := Instance (ID).Command_Issue;
         Local_Int_Status := Instance (ID).Interrupt_Status;
         exit when (Local_Cmd_Issue (0) = False)
                     or (Local_Int_Status.TFES = True);
         Delays.M_Delay (Msec => 1);
      end loop;

      pragma Debug (Now > Timeout, Debug_Ops.Put_Line ("TimeOut!"));
      Check_Error (ID, Error);

      if Error then
         Recover_Errors (ID);
         Success := False;
         return;
      end if;
      Clear.Intr := True;
      Clear_Errors (ID, Clear);

      if (Local_Cmd_Issue (0) = True)
         and (Local_Int_Status.TFES = False)
      then
         pragma Debug (Debug_Ops.Put_Line (
            "AHCI: Timeout during command execution!"));
         Success := False;
         pragma Debug (Debug_Ops.Dump_Port_Regs (ID));
      else
         Success := True;
      end if;

   end Execute;

   -------------------------------------------------------------------------

   procedure Is_Active
     (ID     :     Port_Range;
      Active : out Boolean)
   is
      Status : Port_SATA_Status_Type;
   begin
      Status := Instance (ID).SATA_Status;
      Active := (Status.IPM = Interface_Active)
                and (Status.DET = Present_Established);
   end Is_Active;

   -------------------------------------------------------------------------

   procedure Power_Up (ID : Port_Range)
   is
      Cmd : Port_Command_Status_Type;
   begin
      Cmd := Instance (ID).Command_And_Status;
      Cmd.POD := True;
      Cmd.FRE := True;
      Instance (ID).Command_And_Status := Cmd;
   end Power_Up;

   -------------------------------------------------------------------------

   procedure Reset
     (ID      :     Port_Range;
      Success : out Boolean)
   is
      Reset_SERR : constant Port_SATA_Error_Type
        := (ERR  => Interfaces.Unsigned_16'Last,
            DIAG => Interfaces.Unsigned_16'Last);

      Cmd_List_Running : Boolean;
      Device_Detection : Unsigned_4;
      Command_Status   : Port_Command_Status_Type
                           := Instance (ID).Command_And_Status;
      Sata_Ctrl        : Port_SATA_Control_Type;
   begin
      pragma Debug (Debug_Ops.Put_Line ("Reset Port" &
         SK.Strings.Img (Interfaces.Unsigned_8 (ID))));
      --  Serial ATA AHCI 1.3.1 Specification, section 10.4.2.
      Command_Status.ST := False;
      Instance (ID).Command_And_Status := Command_Status;

      for I in Natural range 1 .. 500 loop
         Cmd_List_Running := Instance (ID).Command_And_Status.CR;
         exit when not Cmd_List_Running;
         Delays.M_Delay (Msec => 1);
      end loop;

      pragma Debug (Cmd_List_Running,
                    Debug_Ops.Put_Line ("Port " & SK.Strings.Img
                      (Item => Interfaces.Unsigned_8 (ID))
                      & ": Command list still running, issuing reset anyway"));

      Sata_Ctrl := Instance (ID).SATA_Control;
      Sata_Ctrl.DET := 1;
      Instance (ID).SATA_Control := Sata_Ctrl;

      Delays.M_Delay (Msec => 1);

      Sata_Ctrl.DET := 0;
      Instance (ID).SATA_Control := Sata_Ctrl;

      for I in Natural range 0 .. 1000 loop
         Device_Detection := Instance (ID).SATA_Status.DET;
         exit when Device_Detection = Present_Established;
         Delays.M_Delay (Msec => 1);
      end loop;

      Instance (ID).SATA_Error := Reset_SERR;

      Success := Device_Detection = Present_Established;
   end Reset;

   -------------------------------------------------------------------------

   procedure Spin_Up (ID : Port_Range)
   is
      Cmd_Status : Port_Command_Status_Type
                     := Instance (ID).Command_And_Status;
      Active     : Boolean;
   begin
      Cmd_Status.SUD := True;
      Instance (ID).Command_And_Status := Cmd_Status;

      for I in Natural range 1 .. 100 loop
         Is_Active (ID, Active);
         exit when Active;
      end loop;

   end Spin_Up;

   -------------------------------------------------------------------------

   procedure Start (ID : Port_Range)
   is
      Status : Port_Command_Status_Type;
   begin
      Status := Instance (ID).Command_And_Status;
      if Status.ST then
         return;
      end if;

      for I in Natural range 1 .. 1000 loop
         Status := Instance (ID).Command_And_Status;
         exit when not Status.CR;
      end loop;

      Status.FRE := True;
      Instance (ID).Command_And_Status := Status;
      Status.ST  := True;
      Instance (ID).Command_And_Status := Status;
   end Start;

   -------------------------------------------------------------------------

   procedure Stop (ID : Port_Range)
   is
      Cmd_List_Running    : Boolean;
      Fis_Receive_Enabled : Boolean;
      Success             : Boolean;
      Status              : Port_Command_Status_Type
                              := Instance (ID).Command_And_Status;
   begin

      --  Serial ATA AHCI 1.3.1 Specification, section 10.1.2.
      Status.ST := False;
      Instance (ID).Command_And_Status := Status;

      for I in Natural range 1 .. 500 loop
         Cmd_List_Running := Instance (ID).Command_And_Status.CR;
         exit when not Cmd_List_Running;
         Delays.M_Delay (Msec => 1);
      end loop;

      for I in Natural range 1 .. 500 loop
         Fis_Receive_Enabled := Instance (ID).Command_And_Status.FRE;
         exit when not Fis_Receive_Enabled;
         Delays.M_Delay (Msec => 1);
      end loop;

      if Fis_Receive_Enabled or Cmd_List_Running then
         pragma Debug (Fis_Receive_Enabled, Debug_Ops.Put_Line (
            "Fis Receive Still enabled"));
         pragma Debug (Cmd_List_Running, Debug_Ops.Put_Line (
            "Cmd_List_Running enabled"));
         Reset (ID, Success);
         if not Success then
            pragma Debug (Debug_Ops.Put_Line ("Unable to reset Port!"));
            --  TODO: HBA Reset
         end if;
      end if;
   end Stop;

end Ahci.Ports;
