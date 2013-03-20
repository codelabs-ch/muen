with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

with SK;

package Skp
is

   --  Subject name.
   subtype Subject_Name_Type is Ada.Strings.Unbounded.Unbounded_String;

   --  Memory access permissions.
   type Memory_Permission_Type is
     (Read_Only,
      Read_Write);

   --  Memory region specification.
   type Memory_Region_Type is private;

   --  Return physical start address of memory region.
   function Get_Physical_Address
     (Region : Memory_Region_Type)
      return SK.Word64;

   --  Return virtual start address of memory region.
   function Get_Virtual_Address
     (Region : Memory_Region_Type)
      return SK.Word64;

   --  Return size of memory region in bytes.
   function Get_Size (Region : Memory_Region_Type) return SK.Word64;

   --  Return access permissions of memory region.
   function Get_Permission
     (Region : Memory_Region_Type)
      return Memory_Permission_Type;

   --  Returns True if the memory region is marked as executable.
   function Is_Executable (Region : Memory_Region_Type) return Boolean;

   --  Memory layout specification.
   type Memory_Layout_Type is private;

   --  Return PML4 address of memory layout.
   function Get_Pml4_Address (Layout : Memory_Layout_Type) return SK.Word64;

   --  Return memory layout region count.
   function Get_Region_Count (Layout : Memory_Layout_Type) return Positive;

   --  Subject specification.
   type Subject_Type is private;

   --  Return subject Id.
   function Get_Id (Subject : Subject_Type) return Natural;

   --  Return subject name.
   function Get_Name (Subject : Subject_Type) return Subject_Name_Type;

   --  Return subject memory layout.
   function Get_Memory_Layout
     (Subject : Subject_Type)
      return Memory_Layout_Type;

   --  SK system policy.
   type Policy_Type is private;

   --  Return subjects count.
   function Get_Subject_Count (Policy : Policy_Type) return Positive;

   --  Iterate over subjects.
   procedure Iterate
     (Policy  : Policy_Type;
      Process : not null access procedure (S : Subject_Type));

   Subject_Not_Found : exception;

private

   function "<" (Left, Right : Subject_Type) return Boolean;

   type Memory_Region_Type is record
      Physical_Address : SK.Word64;
      Virtual_Address  : SK.Word64;
      Size             : SK.Word64;
      Permission       : Memory_Permission_Type;
      Executable       : Boolean;
   end record;

   package Memregion_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Memory_Region_Type);

   type Memory_Layout_Type is record
      Pml4_Address : SK.Word64;
      Regions      : Memregion_Package.List;
   end record;

   type Subject_Type is record
      Id            : Natural;
      Name          : Subject_Name_Type;
      Memory_Layout : Memory_Layout_Type;
   end record;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   type Policy_Type is record
      Subjects : Subjects_Package.Set;
   end record;

end Skp;
