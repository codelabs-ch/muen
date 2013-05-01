with System.Machine_Code;

with Skp;

with SK.Apic;

package body SK.MP
--# own Barrier is Sense, CPU_Sense, Barrier_Count;
is

   type Sense_Array is array (Skp.CPU_Range) of Boolean;

   Sense         : Boolean     := False;
   CPU_Sense     : Sense_Array := Sense_Array'(others => True);
   Barrier_Count : SK.Byte     := 0;
   pragma Atomic (Barrier_Count);

   -------------------------------------------------------------------------

   procedure Get_And_Increment_Barrier (Count : out SK.Byte)
   --# global
   --#    in out Barrier_Count;
   --# derives
   --#    Barrier_Count, Count from Barrier_Count;
   is
      --# hide Get_And_Increment_Barrier;
   begin
      Count := 1;

      System.Machine_Code.Asm
        (Template => "lock xaddb %0, %1",
         Inputs   => (SK.Byte'Asm_Input ("a", Count),
                      SK.Byte'Asm_Input ("m", Barrier_Count)),
         Outputs  => (SK.Byte'Asm_Output ("=a", Count),
                      SK.Byte'Asm_Output ("=m", Barrier_Count)),
         Volatile => True);
   end Get_And_Increment_Barrier;

   -------------------------------------------------------------------------

   procedure Wait_For_All
   --# global
   --#    in out Sense;
   --#    in out CPU_Sense;
   --#    in out Barrier_Count;
   --# derives
   --#    CPU_Sense     from *, Sense &
   --#    Sense         from *, Barrier_Count &
   --#    Barrier_Count from *;
   is
      --# hide Wait_For_All;
      Count : SK.Byte;
      Id    : constant Skp.CPU_Range := Skp.CPU_Range (Apic.Get_ID);
   begin
      CPU_Sense (Id) := not Sense;
      Get_And_Increment_Barrier (Count => Count);

      if Count = SK.Byte (Skp.CPU_Range'Last) then
         Barrier_Count := 0;
         Sense         := CPU_Sense (Id);
      else
         while Sense /= CPU_Sense (Id) loop
            null;
         end loop;
      end if;
   end Wait_For_All;

end SK.MP;
