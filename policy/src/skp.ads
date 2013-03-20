with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

package Skp
is

   subtype Subject_Name_Type is Ada.Strings.Unbounded.Unbounded_String;

   --  Subject specification.
   type Subject_Type is private;

   --  Return subject Id.
   function Get_Id (Subject : Subject_Type) return Natural;

   --  Return subject name.
   function Get_Name (Subject : Subject_Type) return Subject_Name_Type;

   --  SK system policy.
   type Policy_Type is private;

   --  Return subjects count.
   function Get_Subject_Count (Policy : Policy_Type) return Positive;

   --  Iterate over subjects.
   procedure Iterate
     (Policy  : Policy_Type;
      Process : not null access procedure (S : Subject_Type));

private

   function "<" (Left, Right : Subject_Type) return Boolean;

   type Subject_Type is record
      Id   : Natural;
      Name : Subject_Name_Type;
   end record;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   type Policy_Type is record
      Subjects : Subjects_Package.Set;
   end record;

end Skp;
