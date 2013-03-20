package body Skp
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Subject_Type) return Boolean
   is
   begin
      return Left.Id < Right.Id;
   end "<";

   -------------------------------------------------------------------------

   function Get_Id (Subject : Subject_Type) return Natural
   is
   begin
      return Subject.Id;
   end Get_Id;

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

   function Get_Pml4_Address (Layout : Memory_Layout_Type) return SK.Word64
   is
   begin
      return Layout.Pml4_Address;
   end Get_Pml4_Address;

   -------------------------------------------------------------------------

   function Get_Subject
     (Policy : Policy_Type;
      Id     : Natural)
      return Subject_Type
   is
      use type Subjects_Package.Cursor;

      Pos : Subjects_Package.Cursor := Policy.Subjects.First;
   begin
      while Pos /= Subjects_Package.No_Element loop
         if Subjects_Package.Element (Position => Pos).Id = Id then
            return Subjects_Package.Element (Position => Pos);
         end if;

         Subjects_Package.Next (Position => Pos);
      end loop;

      raise Subject_Not_Found with "No subject with ID" & Id'Img & " found";
   end Get_Subject;

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

   -------------------------------------------------------------------------

   function Get_Subject_Count (Policy : Policy_Type) return Positive
   is
   begin
      return Positive (Policy.Subjects.Length);
   end Get_Subject_Count;

end Skp;
