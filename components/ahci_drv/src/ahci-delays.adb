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

with Interfaces;

with SK.CPU;

package body Ahci.Delays
is

   use type Interfaces.Unsigned_64;

   --  Suspend execution of caller for at least the specified amount of seconds
   --  scaled by given unit.
   procedure Sleep
     (Amount : Interfaces.Unsigned_64;
      Unit   : Interfaces.Unsigned_64)
   with
      Global => (Input  => (Musinfo.Instance.State,
                            Musinfo.Instance.Scheduling_Info),
                 In_Out => Time_Passes),
      Pre    => Musinfo.Instance.Is_Valid and then Unit > 0;

   --  Returns Timestamp counter frequency in Hz.
   function TSC_Hz return Interfaces.Unsigned_64
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Suspend execution of caller until the given deadline specified in TSC
   --  ticks has passed.
   procedure Sleep_Until (Deadline : Interfaces.Unsigned_64)
   with
      Global => (Proof_In => Musinfo.Instance.State,
                 Input    => Musinfo.Instance.Scheduling_Info,
                 In_Out   => Time_Passes),
      Pre    => Musinfo.Instance.Is_Valid;

   -------------------------------------------------------------------------

   procedure M_Delay (Msec : Natural)
   is
   begin
      Sleep (Amount => Interfaces.Unsigned_64 (Msec),
             Unit   => 1000);
   end M_Delay;

   -------------------------------------------------------------------------

   procedure Sleep
     (Amount : Interfaces.Unsigned_64;
      Unit   : Interfaces.Unsigned_64)
   is
      Now   : constant Interfaces.Unsigned_64
        := Musinfo.Instance.TSC_Schedule_End;
      Ticks : constant Interfaces.Unsigned_64
        := (Amount * TSC_Hz + (Unit - 1)) / Unit;
   begin
      Sleep_Until (Deadline => Now + Ticks);
   end Sleep;

   -------------------------------------------------------------------------

   procedure Sleep_Until (Deadline : Interfaces.Unsigned_64)
   with
      SPARK_Mode => Off
   is
      Now : Interfaces.Unsigned_64;
   begin
      loop
         Now := Musinfo.Instance.TSC_Schedule_Start;
         exit when Now >= Deadline;
         SK.CPU.Pause;
      end loop;
   end Sleep_Until;

   -------------------------------------------------------------------------

   function TSC_Hz return Interfaces.Unsigned_64
   is (Musinfo.Instance.TSC_Khz * 1000);

   -------------------------------------------------------------------------

   procedure U_Delay (Usec : Natural)
   is
   begin
      Sleep (Amount => Interfaces.Unsigned_64 (Usec),
             Unit   => 1_000_000);
   end U_Delay;

end Ahci.Delays;
