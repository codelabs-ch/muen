--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.Hypercall;

pragma $Release_Warnings (Off, "unit * is not referenced");
with Debuglog.Client;
pragma $Release_Warnings (On, "unit * is not referenced");

with Musinfo.Utils;
with Musinfo.Instance;

with Loader.Process_Target;

with Sl_Component.Events;

procedure Sl
is
begin
   pragma Debug (Debuglog.Client.Put_Line (Item => "SL subject running"));

   if not Musinfo.Instance.Is_Valid then
      pragma Debug
        (Debuglog.Client.Put_Line
           (Item => "Error: Own sinfo data not valid, halting"));
      SK.CPU.Stop;
   end if;

   loop
      pragma Debug (Debuglog.Client.Put_Line
                    (Item => "Resetting managed subject(s)"));

      declare
         use type Musinfo.Resource_Kind;

         Pattern  : constant String := "monitor_sinfo";
         Iter     : Musinfo.Utils.Resource_Iterator_Type
           := Musinfo.Instance.Create_Resource_Iterator;
         Resource : Musinfo.Resource_Type;
         Success  : Boolean := False;
      begin
         Process_Loop :
         while Musinfo.Instance.Has_Element (Iter => Iter) loop
            Resource := Musinfo.Instance.Element (Iter => Iter);

            if Resource.Kind = Musinfo.Res_Memory
              and then
                Musinfo.Utils.Names_Match
                  (N1    => Resource.Name,
                   N2    => Musinfo.Utils.To_Name (Str => Pattern),
                   Count => Pattern'Length)
            then
               Loader.Process_Target.Process
                 (Sinfo_Mem => Resource,
                  Success   => Success);
               exit Process_Loop;
            end if;

            Musinfo.Instance.Next (Iter => Iter);
            pragma Loop_Invariant (Musinfo.Instance.Belongs_To (Iter => Iter));
         end loop Process_Loop;

         if not Success then
            pragma Debug
              (Debuglog.Client.Put_Line
                 (Item => "Error: Reset of subject failed"));
            SK.CPU.Stop;
         end if;
      end;

      SK.Hypercall.Trigger_Event (Number => Sl_Component.Events.Start_ID);
   end loop;
end Sl;
