--
--  Copyright (C) 2014, 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.Bitops;
with SK.Strings;

with Mudm.Config;

with Debug_Ops;

package body Exit_Handlers.EPT_Violation
is

   use Subject_Info;

   -------------------------------------------------------------------------

   --  Return EPT violation information from exit qualification, as specified
   --  by Intel SDM Vol. 3C, section 27.2.1, table 27-7.
   function To_EPTV_Info
     (Qualification : SK.Word64)
      return Types.EPTV_Info_Type
   is
      Info : Types.EPTV_Info_Type;
   begin
      Info.Read              := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 0);
      Info.Write             := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 1);
      Info.Instruction_Fetch := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 2);
      Info.Is_Readable       := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 3);
      Info.Is_Writable       := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 4);
      Info.Valid_Address     := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 7);
      Info.Is_Linear_Access  := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 8);
      Info.NMI_Blocking      := SK.Bitops.Bit_Test (Value => Qualification,
                                                    Pos   => 12);
      return Info;
   end To_EPTV_Info;

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      use type SK.Word64;

      Exit_Q : constant SK.Word64 := Subject_Info.State.Exit_Qualification;
      GPA    : constant SK.Word64 := State.Guest_Phys_Addr;

      Info : constant Types.EPTV_Info_Type
        := To_EPTV_Info (Qualification => Exit_Q);
   begin
      Action := Types.Subject_Halt;

      if GPA in Mudm.Config.MMConf_Region then
         Action := Types.Subject_Continue;

         declare
            EAX    : SK.Word32;
            RAX    : constant SK.Word64        := Subject_Info.State.Regs.RAX;
            Offset : constant Mudm.Offset_Type := Mudm.Offset_Type'Mod (GPA);
            SID    : constant Musinfo.SID_Type := Musinfo.SID_Type
              (Interfaces.Shift_Right
                 (Value  => GPA - Mudm.Config.MMConf_Base_Address,
                  Amount => 12));
         begin
            if Info.Read then
               Mudm.Client.Pciconf_Emulate_Read
                 (SID    => SID,
                  Offset => Offset,
                  Result => EAX);
               Subject_Info.State.Regs.RAX := SK.Word64 (EAX);
            else
               EAX := SK.Word32'Mod (RAX);

               Mudm.Client.Pciconf_Emulate_Write
                 (SID    => SID,
                  Offset => Offset,
                  Value  => EAX);
            end if;
         end;
      end if;

      pragma Debug (GPA not in Mudm.Config.MMConf_Region,
                    Debug_Ops.Put_String (Item => "Invalid "));
      pragma Debug (GPA not in Mudm.Config.MMConf_Region and then Info.Read,
                    Debug_Ops.Put_String (Item => "read"));
      pragma Debug (GPA not in Mudm.Config.MMConf_Region and then Info.Write,
                    Debug_Ops.Put_String (Item => "write"));
      pragma Debug (GPA not in Mudm.Config.MMConf_Region, Debug_Ops.Put_Line
                    (Item => " access at guest physical address "
                     & SK.Strings.Img (GPA)));
   end Process;

end Exit_Handlers.EPT_Violation;
