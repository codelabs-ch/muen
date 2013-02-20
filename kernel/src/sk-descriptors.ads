--# inherit
--#    SK;
package SK.Descriptors
is

   --  Pseudo Descriptor type, see Intel SDM vol. 3A, chapter 3.5.1.
   type Pseudo_Descriptor_Type is record
      Limit : SK.Word16;
      Base  : SK.Word64;
   end record;

private

   for Pseudo_Descriptor_Type use record
      Limit at 0 range 0 .. 15;
      Base  at 2 range 0 .. 63;
   end record;
   for Pseudo_Descriptor_Type'Size use 80;

end SK.Descriptors;
