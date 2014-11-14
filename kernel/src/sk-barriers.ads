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

package SK.Barriers
is

   type Sense_Barrier_Type is private;

   --  Wait on barrier until it is released.
   procedure Wait (Barrier : in out Sense_Barrier_Type)
   with
      Depends => (Barrier =>+ null);

   pragma $Prove_Warnings  --  Workaround for [NB11-009]
     (Off, "*Barrier""",
      Reason => "Barrier is actually mode out but must be in out to enable use"
      & " of Async_Writer aspect for barrier instances");
   --  Initialize barrier with the given size. The size of the barrier
   --  specifies how many CPUs must wait on the barrier to be released.
   procedure Initialize
     (Barrier : in out Sense_Barrier_Type;
      Size    :        SK.Byte)
   with
      Depends => (Barrier =>+ Size);
   pragma Annotate (GNATprove, Intentional,
                    "missing dependency ""null => Barrier""",
                    "Workaround for [NB11-009]");
   pragma Annotate (GNATprove, Intentional,
                    "incorrect dependency ""Barrier => Barrier""",
                    "Workaround for [NB11-009]");
   pragma $Prove_Warnings (On, "*Barrier""");

private

   type Sense_Barrier_Type is record
      Size       : SK.Byte with Atomic;
      Sense      : Boolean with Atomic;
      Wait_Count : SK.Byte with Atomic;
   end record
     with Volatile;

end SK.Barriers;
