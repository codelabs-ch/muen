with Ada.Containers.Ordered_Sets;

package Skp
is

   --  Subject specification.
   type Subject_Type is private;

   --  SK system policy.
   type Policy_Type is private;

   --  Return subjects count.
   function Get_Subject_Count (Policy : Policy_Type) return Positive;

private

   function "<" (Left, Right : Subject_Type) return Boolean;

   type Subject_Type is record
      Id : Natural;
   end record;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   type Policy_Type is record
      Subjects : Subjects_Package.Set;
   end record;

end Skp;
