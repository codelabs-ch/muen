with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with SK;

package Skp
is

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

   subtype Memory_Layout_Type is Memregion_Package.List;

   type IO_Port_Range is record
      Start_Port : SK.Word16;
      End_Port   : SK.Word16;
   end record;

   package Ports_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => IO_Port_Range);

   subtype IO_Ports_Type is Ports_Package.List;

   type Initial_State_Type is record
      Stack_Address : SK.Word64;
      Entry_Point   : SK.Word64;
   end record;

   type Binary_Ref_Type is record
      Name             : Ada.Strings.Unbounded.Unbounded_String;
      Physical_Address : SK.Word64;
   end record;

   type Trap_Type is
     (Exception_Or_NMI,
      External_Interrupt);

   type Trap_Table_Entry_Type is record
      Trap        : Trap_Type;
      Dst_Subject : Ada.Strings.Unbounded.Unbounded_String;
      Dst_Vector  : Integer := -1;
   end record;

   package Traps_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Trap_Type,
      Element_Type => Trap_Table_Entry_Type);

   subtype Trap_Table_Type is Traps_Package.Map;

   type Subject_Type is record
      Id                : Natural;
      Name              : Ada.Strings.Unbounded.Unbounded_String;
      Pml4_Address      : SK.Word64;
      IO_Bitmap_Address : SK.Word64;
      Init_State        : Initial_State_Type;
      Memory_Layout     : Memory_Layout_Type;
      Binary            : Binary_Ref_Type;
      IO_Ports          : IO_Ports_Type;
      Trap_Table        : Trap_Table_Type;
   end record;

   function "<" (Left, Right : Subject_Type) return Boolean;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   subtype Subjects_Type is Subjects_Package.Set;

   --  Returns subject id of subject with given name. If no subject with
   --  specified name exists, -1 is returned.
   function Get_Id
     (Subjects : Subjects_Type;
      Name     : Ada.Strings.Unbounded.Unbounded_String)
      return Integer;

   package Binary_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "<"          => Ada.Strings.Unbounded."<",
      "="          => Ada.Strings.Unbounded."=");

   subtype Binaries_Type is Binary_Package.Map;

   type Kernel_Type is record
      Stack_Address    : SK.Word64;
      Pml4_Address     : SK.Word64;
      CPU_Page_Address : SK.Word64;
      Memory_Layout    : Memory_Layout_Type;
   end record;

   package Owners_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Natural);

   subtype Owners_Type is Owners_Package.List;

   type Device_Type is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Memory_Layout : Memory_Layout_Type;
      IO_Ports      : IO_Ports_Type;
      IRQ           : Integer := -1;
      Owners        : Owners_Type;
   end record;

   package Devices_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Device_Type,
      "<"          => Ada.Strings.Unbounded."<");

   type Processor_Type is record
      Logical_CPUs   : Positive;
      Speed          : Positive;
      VMX_Timer_Rate : Natural;
   end record;

   type Hardware_Type is record
      Processor : Processor_Type;
      Devices   : Devices_Package.Map;
   end record;

   type Minor_Frame_Type is record
      Subject_Id : Natural;
      Ticks      : Positive;
   end record;

   package Minor_Frames_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Minor_Frame_Type);

   subtype CPU_Type is Minor_Frames_Package.List;

   package CPU_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => CPU_Type,
      "="          => Minor_Frames_Package."=");

   subtype Major_Frame_Type is CPU_Package.Vector;

   package Major_Frames_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Major_Frame_Type,
      "="          => CPU_Package."=");

   subtype Major_Frames_Type is Major_Frames_Package.List;

   type Scheduling_Type is record
      Tick_Rate    : Positive;
      Major_Frames : Major_Frames_Type;
   end record;

   type Policy_Type is record
      Vmxon_Address      : SK.Word64;
      Vmcs_Start_Address : SK.Word64;
      Hardware           : Hardware_Type;
      Kernel             : Kernel_Type;
      Subjects           : Subjects_Type;
      Binaries           : Binaries_Type;
      Scheduling         : Scheduling_Type;
   end record;

   IO_Error : exception;

end Skp;
