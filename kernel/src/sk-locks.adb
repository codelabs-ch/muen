with System.Machine_Code;

package body SK.Locks
--# own
--#    State is Lock;
is

   type Spin_Lock_Type is record
      Locked : SK.Byte;
   end record;

   Lock : Spin_Lock_Type;

   -------------------------------------------------------------------------

   procedure Spin_Lock
   --# global
   --#    in out Lock;
   --# derives
   --#    Lock from *;
   is
      --# hide Spin_Lock;

      Result : SK.Byte;
   begin
      loop
         System.Machine_Code.Asm
           (Template => "mov $1, %%eax; lock xchgl %%eax, (%%edx)",
            Outputs  => (SK.Byte'Asm_Output ("=a", Result)),
            Inputs   => (System.Address'Asm_Input ("d", Lock.Locked'Address)));

         if Result = 0 then
            exit;
         end if;
      end loop;
   end Spin_Lock;

   -------------------------------------------------------------------------

   procedure Unlock
   --# global
   --#    in out Lock;
   --# derives
   --#    Lock from *;
   is
      --# hide Unlock;
   begin
      System.Machine_Code.Asm
        (Template => "movq $0, %0",
         Outputs  => (SK.Byte'Asm_Output ("=m", Lock.Locked)),
         Volatile => True);
   end Unlock;

begin
   Lock.Locked := 0;
end SK.Locks;
