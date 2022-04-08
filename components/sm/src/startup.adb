--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with SK.Hypercall;
with SK.Strings;

with Musinfo.Utils;
with Mucontrol.Status;

with Debug_Ops;

package body Startup
with
   Refined_State => (State => Linux_Status)
is

   Reset_Event_Name  : constant String := "reset_linux";
   Load_Event_Name   : constant String := "load_linux";
   Linux_Status_Name : constant String := "status_linux";

   Linux_Status_Address : constant := 16#0200_0000#;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Linux_Status : Mucontrol.Status.Status_Interface_Type
   with
      Async_Writers,
      Address => System'To_Address (Linux_Status_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Setup_Monitored_Subject (Success : out Boolean)
   with
      Refined_Global  => ((Input  => (Linux_Status, Musinfo.Instance.State),
                           In_Out => X86_64.State)),
      Refined_Depends => (Success      => (Linux_Status,
                                           Musinfo.Instance.State),
                          X86_64.State =>+ Musinfo.Instance.State)
   is
      use type Musinfo.Resource_Type;

      Event : Musinfo.Resource_Type;
   begin
      Success := True;
      Event := Musinfo.Instance.Resource_By_Name
        (Name => Musinfo.Utils.To_Name (Str => Reset_Event_Name),
         Kind => Musinfo.Res_Event);
      if Event = Musinfo.Null_Resource then
         Debug_Ops.Put_Line (Item => "No " & Reset_Event_Name
                             & " event present");
         return;
      end if;
      SK.Hypercall.Trigger_Event (Number => Event.Evt_Data.Value);
      Debug_Ops.Put_Line (Item => "Linux reset");

      Event := Musinfo.Instance.Resource_By_Name
        (Name => Musinfo.Utils.To_Name (Str => Load_Event_Name),
         Kind => Musinfo.Res_Event);
      if Event = Musinfo.Null_Resource then
         Debug_Ops.Put_Line (Item => "No " & Load_Event_Name
                             & " event present");
         return;
      end if;
      SK.Hypercall.Trigger_Event (Number => Event.Evt_Data.Value);

      declare
         use type SK.Word64;

         Mem_Res : constant Musinfo.Resource_Type
           := Musinfo.Instance.Resource_By_Name
             (Name => Musinfo.Utils.To_Name (Str => Linux_Status_Name),
              Kind => Musinfo.Res_Memory);
         Cur_State, Unused_Diag : SK.Word64;
      begin
         if Mem_Res = Musinfo.Null_Resource
           or else Mem_Res.Mem_Data.Address /= Linux_Status_Address
         then
            Debug_Ops.Put_Line
              (Item => "Linux status page not mapped at "
               & SK.Strings.Img (SK.Word64 (Linux_Status_Address)));
            if Mem_Res /= Musinfo.Null_Resource then
               Debug_Ops.Put_Line
                 (Item => "Mapped at "
                  & SK.Strings.Img (Mem_Res.Mem_Data.Address));
            end if;
            Success := False;
            return;
         end if;

         Cur_State   := SK.Word64 (Linux_Status.State);
         Unused_Diag := SK.Word64 (Linux_Status.Diagnostics);

         Success := (Cur_State = SK.Word64 (Mucontrol.Status.STATE_RUNNING));
         if Success then
            Debug_Ops.Put_Line (Item => "Linux loaded");
         else
            Debug_Ops.Put_Line
              (Item => "Unexpected status after loading Linux");
            Debug_Ops.Put_Line (Item => "State      : "
                                & SK.Strings.Img (Item => Cur_State));
            Debug_Ops.Put_Line (Item => "Diagnostics: "
                                & SK.Strings.Img (Item => Unused_Diag));
         end if;
      end;
   end Setup_Monitored_Subject;

end Startup;
