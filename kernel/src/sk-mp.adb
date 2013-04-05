package body SK.MP
is

   --  CPUs online.
   CPU_Online_Count : SK.Byte := 0;
   pragma Atomic (CPU_Online_Count);

   -------------------------------------------------------------------------

   function Get_CPU_Count return SK.Byte
   is
   begin
      return CPU_Online_Count;
   end Get_CPU_Count;

   -------------------------------------------------------------------------

   procedure Increment_CPU_Count
   is
   begin
      CPU_Online_Count := CPU_Online_Count + 1;
   end Increment_CPU_Count;

   -------------------------------------------------------------------------

   procedure Wait_For_AP_Processors
   is
      --# hide Wait_For_AP_Processors;
   begin

      --  Spin until all APs are online.

      while CPU_Online_Count /= CPU_Count loop
         null;
      end loop;
   end Wait_For_AP_Processors;

end SK.MP;
