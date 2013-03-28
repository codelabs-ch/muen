with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

with SK;

package Skp
is

   --  Subject name.
   subtype Subject_Name_Type is Ada.Strings.Unbounded.Unbounded_String;

   type Memory_Region_Type is record
      Physical_Address : SK.Word64;
      Virtual_Address  : SK.Word64;
      Size             : SK.Word64;
      Alignment        : SK.Word64;
      Writable         : Boolean;
      Executable       : Boolean;
   end record;

   package Memregion_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Memory_Region_Type);

   type Memory_Layout_Type is record
      Pml4_Address : SK.Word64;
      Regions      : Memregion_Package.List;
   end record;

   type IO_Port_Range is record
      Start_Port : SK.Word16;
      End_Port   : SK.Word16;
   end record;

   package Ports_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => IO_Port_Range);

   type IO_Ports_Type is record
      IO_Bitmap_Address : SK.Word64;
      Ranges            : Ports_Package.List;
   end record;

   type Subject_Type is record
      Id            : Natural;
      Name          : Subject_Name_Type;
      Memory_Layout : Memory_Layout_Type;
      IO_Ports      : IO_Ports_Type;
   end record;

   function "<" (Left, Right : Subject_Type) return Boolean;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   type Kernel_Type is record
      Memory_Layout : Memory_Layout_Type;
   end record;

   type Policy_Type is record
      Vmxon_Address : SK.Word64;
      Kernel        : Kernel_Type;
      Subjects      : Subjects_Package.Set;
   end record;

end Skp;
