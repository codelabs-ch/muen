with Example_Component.Memory_Arrays;
with Interfaces; use Interfaces;
package Muenblock_Example.Write_Ops_Helper
is
   procedure Run_Instance
      (SHM_Array_Index :     Positive;
       Sector_Size     :     Interfaces.Unsigned_64;
       Success         : out Boolean)
   with Pre => Musinfo.Instance.Is_Valid and Sector_Size > 0 and
               SHM_Array_Index in 1 .. Example_Component.Memory_Arrays.Blockdev_Shm2_Element_Count;

end Muenblock_Example.Write_Ops_Helper;