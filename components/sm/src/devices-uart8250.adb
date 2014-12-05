--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Devices.UART8250
with
   Refined_State => (State => Com1)
is

   type UART8250_Type is record
      IER, IIR : SK.Byte;
   end record;

   Null_UART8250 : constant UART8250_Type
     := (IER => 0,
         IIR => 1);

   type Com_State_Type is record
      Base_Port : SK.Word16;
      Event     : SK.Byte;
      UART      : UART8250_Type;
   end record;

   Com1 : Com_State_Type
     := (Base_Port => Com1_Port_Range'First,
         Event     => 0,
         UART      => Null_UART8250);

   Register_Base : constant := 0;
   Register_IER  : constant := 1;
   Register_IIR  : constant := 2;
   Register_LSR  : constant := 5;

   LSR_THR_Empty_Line_Idle : constant := 2#0110_0000#;

   -------------------------------------------------------------------------

   procedure Emulate
     (Info :     Types.IO_Info_Type;
      Halt : out Boolean)
   with
      Refined_Global  => (In_Out => (Com1, Subject_Info.State,
                                     Debuglog.Client.State)),
      Refined_Depends =>
        ((Com1, Subject_Info.State, Debuglog.Client.State) =>+
           (Com1, Info, Subject_Info.State),
         Halt => null)
   is
      use type SK.Byte;
      use type SK.Word16;
      use type SK.Word64;

      Register : constant SK.Word16 := Info.Port_Number - Com1.Base_Port;
      RAX      : constant SK.Word64 := Subject_Info.State.Regs.RAX;
   begin
      Halt := False;

      case Info.Direction is
         when Types.Dir_In =>
            case Register is
               when Register_IER =>
                  Subject_Info.State.Regs.RAX :=
                    (RAX and not 16#ff#) or
                    SK.Word64 (Com1.UART.IER);
               when Register_IIR =>
                  Subject_Info.State.Regs.RAX :=
                    (RAX and not 16#ff#) or
                    SK.Word64 (Com1.UART.IIR);
                  Com1.UART.IIR := 1;
               when Register_LSR =>
                  Subject_Info.State.Regs.RAX :=
                    (RAX and not 16#ff#) or LSR_THR_Empty_Line_Idle;
               when others =>
                  Subject_Info.State.Regs.RAX := RAX and not 16#ff#;
            end case;
         when Types.Dir_Out =>
            case Register is
               when Register_Base =>
                  Debuglog.Client.Put
                    (Item => (Character'Val (SK.Byte'Mod (RAX and 16#ff#))));
               when Register_IER =>
                  Com1.UART.IER := SK.Byte'Mod (RAX and 16#ff#);
               when others =>
                  null;
            end case;
      end case;
   end Emulate;

end Devices.UART8250;
