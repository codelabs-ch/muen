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

   function Get_Name (Subject : Subject_Type) return Subject_Name_Type
   is
   begin
      return Subject.Name;
   end Get_Name;

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
