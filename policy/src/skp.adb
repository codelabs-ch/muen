package body Skp
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Subject_Type) return Boolean
   is
   begin
      return Left.Id < Right.Id;
   end "<";

   -------------------------------------------------------------------------

   function Get_Bitmap_Address (Ports : IO_Ports_Type) return SK.Word64
   is
   begin
      return Ports.IO_Bitmap_Address;
   end Get_Bitmap_Address;

   -------------------------------------------------------------------------

   function Get_End (Port_Range : IO_Port_Range) return SK.Word16
   is
   begin
      return Port_Range.End_Port;
   end Get_End;

   -------------------------------------------------------------------------

   function Get_Id (Subject : Subject_Type) return Natural
   is
   begin
      return Subject.Id;
   end Get_Id;

   -------------------------------------------------------------------------

   function Get_IO_Ports (Subject : Subject_Type) return IO_Ports_Type
   is
   begin
      return Subject.IO_Ports;
   end Get_IO_Ports;

   -------------------------------------------------------------------------

   function Get_Memory_Layout
     (Subject : Subject_Type)
      return Memory_Layout_Type
   is
   begin
      return Subject.Memory_Layout;
   end Get_Memory_Layout;

   -------------------------------------------------------------------------

   function Get_Name (Subject : Subject_Type) return Subject_Name_Type
   is
   begin
      return Subject.Name;
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Permission
     (Region : Memory_Region_Type)
      return Memory_Permission_Type
   is
   begin
      return Region.Permission;
   end Get_Permission;

   -------------------------------------------------------------------------

   function Get_Physical_Address
     (Region : Memory_Region_Type)
      return SK.Word64
   is
   begin
      return Region.Physical_Address;
   end Get_Physical_Address;

   -------------------------------------------------------------------------

   function Get_Pml4_Address (Layout : Memory_Layout_Type) return SK.Word64
   is
   begin
      return Layout.Pml4_Address;
   end Get_Pml4_Address;

   -------------------------------------------------------------------------

   function Get_Region_Count (Layout : Memory_Layout_Type) return Positive
   is
   begin
      return Positive (Layout.Regions.Length);
   end Get_Region_Count;

   -------------------------------------------------------------------------

   function Get_Size (Region : Memory_Region_Type) return SK.Word64
   is
   begin
      return Region.Size;
   end Get_Size;

   -------------------------------------------------------------------------

   function Get_Start (Port_Range : IO_Port_Range) return SK.Word16
   is
   begin
      return Port_Range.Start_Port;
   end Get_Start;

   -------------------------------------------------------------------------

   function Get_Subject_Count (Policy : Policy_Type) return Positive
   is
   begin
      return Positive (Policy.Subjects.Length);
   end Get_Subject_Count;

   -------------------------------------------------------------------------

   function Get_Virtual_Address
     (Region : Memory_Region_Type)
      return SK.Word64
   is
   begin
      return Region.Virtual_Address;
   end Get_Virtual_Address;

   -------------------------------------------------------------------------

   function Is_Executable (Region : Memory_Region_Type) return Boolean
   is
   begin
      return Region.Executable;
   end Is_Executable;

   -------------------------------------------------------------------------

   procedure Iterate
     (Layout  : Memory_Layout_Type;
      Process : not null access procedure (R : Memory_Region_Type))
   is
      procedure Dispatch (Pos : Memregion_Package.Cursor)
      is
      begin
         Process (R => Memregion_Package.Element (Position => Pos));
      end Dispatch;
   begin
      Layout.Regions.Iterate (Process => Dispatch'Access);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Iterate
     (Policy  : Policy_Type;
      Process : not null access procedure (S : Subject_Type))
   is
      procedure Dispatch (Pos : Subjects_Package.Cursor)
      is
      begin
         Process (S => Subjects_Package.Element (Position => Pos));
      end Dispatch;
   begin
      Policy.Subjects.Iterate (Process => Dispatch'Access);
   end Iterate;

end Skp;
