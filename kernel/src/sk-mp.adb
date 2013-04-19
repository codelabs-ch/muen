with System.Machine_Code;

with Skp;

package body SK.MP
is

   Barrier : SK.Byte := 0;
   pragma Atomic (Barrier);

   -------------------------------------------------------------------------

   procedure Increment_Barrier
   --# global
   --#    in out Barrier;
   --# derives
   --#    Barrier from *;
   is
      --# hide Increment_Barrier;
   begin
      System.Machine_Code.Asm
        (Template => "lock incb %0",
         Inputs   => (SK.Byte'Asm_Input ("m", Barrier)),
         Volatile => True);
   end Increment_Barrier;

   -------------------------------------------------------------------------

   procedure Reset_Barrier
   is
   begin
      Barrier := 0;
   end Reset_Barrier;

   -------------------------------------------------------------------------

   procedure Wait_For_All
   is
      --# hide Wait_For_All;
   begin
      Increment_Barrier;

      --  Wait until all CPUs are blocked by the barrier.

      while Barrier <= SK.Byte (Skp.CPU_Range'Last) loop
         null;
      end loop;
   end Wait_For_All;

end SK.MP;
