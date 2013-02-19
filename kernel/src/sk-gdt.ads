with SK.Descriptors;

--# inherit
--#    SK.Descriptors;
package SK.GDT
--# own
--#    in GDT_Pointer;
is

   --  Return GDT pointer.
   function Get_GDT_Pointer return Descriptors.Pseudo_Descriptor_Type;
   --# global
   --#    GDT_Pointer;

end SK.GDT;
