with Example_Component.Memory_Arrays;
with Muenblock_Example.Write_Ops_Helper;

package body Muenblock_Example.Write_Ops
is

   procedure Run
     (Sector_Size :     Interfaces.Unsigned_64;
      Success     : out Boolean)
   is
   begin
      Success := False;
      for I in 1 .. Example_Component.Memory_Arrays.Blockdev_Shm2_Element_Count loop
         Write_Ops_Helper.Run_Instance (SHM_Array_Index => I, Sector_Size => Sector_Size, Success => Success);
         if not Success
         then
            return;
         end if;
      end loop;
   end Run;

end Muenblock_Example.Write_Ops;
