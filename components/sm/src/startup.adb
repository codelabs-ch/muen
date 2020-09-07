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

with SK.Hypercall;

with Musinfo.Utils;

with Debug_Ops;

package body Startup
is

   Reset_Event_Name : constant String := "reset_linux";
   Load_Event_Name  : constant String := "load_linux";

   -------------------------------------------------------------------------

   procedure Setup_Monitored_Subject (Success : out Boolean)
   is
      use type Musinfo.Resource_Type;

      Event : Musinfo.Resource_Type;
   begin
      Success := True;
      Event := Musinfo.Instance.Resource_By_Name
        (Name => Musinfo.Utils.To_Name (Str => Reset_Event_Name),
         Kind => Musinfo.Res_Event);
      if Event = Musinfo.Null_Resource then
         pragma Debug (Debug_Ops.Put_Line (Item => "No " & Reset_Event_Name
                                           & " event present"));
         return;
      end if;
      SK.Hypercall.Trigger_Event (Number => Event.Evt_Data.Value);
      pragma Debug (Debug_Ops.Put_Line (Item => "Linux reset"));

      Event := Musinfo.Instance.Resource_By_Name
        (Name => Musinfo.Utils.To_Name (Str => Load_Event_Name),
         Kind => Musinfo.Res_Event);
      if Event = Musinfo.Null_Resource then
         pragma Debug (Debug_Ops.Put_Line (Item => "No " & Load_Event_Name
                                           & " event present"));
         return;
      end if;
      SK.Hypercall.Trigger_Event (Number => Event.Evt_Data.Value);
      pragma Debug (Debug_Ops.Put_Line (Item => "Linux loaded"));
   end Setup_Monitored_Subject;

end Startup;
