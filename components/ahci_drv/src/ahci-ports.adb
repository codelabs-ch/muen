--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Ahci.Ports
is

   Device_Not_Present  : constant Unsigned_4 := 16#0#;
   Present_Established : constant Unsigned_4 := 16#3#;

   Interface_Active    : constant Unsigned_4 := 16#1#;
   Interface_Suspend   : constant Unsigned_4 := 16#2#;
   Interface_Slumber   : constant Unsigned_4 := 16#6#;
   Interface_Sleep     : constant Unsigned_4 := 16#8#;

   -------------------------------------------------------------------------

   procedure Enable
     (ID      :     Port_Range;
      Success : out Boolean)
   is
      Status : Port_SATA_Status_Type;
   begin
      Status := Instance (ID).SATA_Status;

      if Status.DET = Device_Not_Present then
         Success := False;
         return;
      end if;

      case Status.IPM is
         when Interface_Suspend
            | Interface_Slumber
            | Interface_Sleep   =>
            Instance (ID).Command_And_Status.ICC := Interface_Active;

            for I in Natural range 1 .. 10 loop
               Status := Instance (ID).SATA_Status;
               exit when Status.DET = Present_Established
                 and then Status.IPM = Interface_Active;
               Delays.M_Delay (Msec => 1);
            end loop;
         when others => null;
      end case;

      Success := Status.DET = Present_Established
        and Status.IPM = Interface_Active;
   end Enable;

   -------------------------------------------------------------------------

   procedure Reset
     (ID      :     Port_Range;
      Success : out Boolean)
   is
      Reset_SERR          : constant Port_SATA_Error_Type
        := (ERR  => Interfaces.Unsigned_16'Last,
            DIAG => Interfaces.Unsigned_16'Last);

      Cmd_List_Running : Boolean;
      Device_Detection : Unsigned_4;
   begin

      --  Serial ATA AHCI 1.3.1 Specification, section 10.4.2.

      Instance (ID).Command_And_Status.ST := False;

      for I in Natural range 1 .. 500 loop
         Cmd_List_Running := Instance (ID).Command_And_Status.CR;
         exit when not Cmd_List_Running;
         Delays.M_Delay (Msec => 1);
      end loop;

      pragma Debug (Cmd_List_Running,
                    Debug_Ops.Put_Line ("Port " & SK.Strings.Img
                      (Item => Interfaces.Unsigned_8 (ID))
                      & ": Command list still running, issuing reset anyway"));

      Instance (ID).SATA_Control.DET := 1;

      Delays.M_Delay (Msec => 1);

      Instance (ID).SATA_Control.DET := 0;

      for I in Natural range 0 .. 1000 loop
         Device_Detection := Instance (ID).SATA_Status.DET;
         exit when Device_Detection = Present_Established;
         Delays.M_Delay (Msec => 1);
      end loop;

      Instance (ID).SATA_Error := Reset_SERR;

      Success := Device_Detection = Present_Established;
   end Reset;

   -------------------------------------------------------------------------

   procedure Stop (ID : Port_Range)
   is
   begin
      Instance (ID).Command_And_Status.ST := False;
   end Stop;

end Ahci.Ports;
