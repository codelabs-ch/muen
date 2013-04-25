package body Skp
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Subject_Type) return Boolean
   is
   begin
      return Left.Id < Right.Id;
   end "<";

   -------------------------------------------------------------------------

   function Get_Id
     (Subjects : Subjects_Type;
      Name     : Ada.Strings.Unbounded.Unbounded_String)
      return Integer
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      S_Pos : Subjects_Package.Cursor := Subjects.First;
   begin
      while Subjects_Package.Has_Element (Position => S_Pos) loop
         declare
            Subj : constant Subject_Type := Subjects_Package.Element
              (Position => S_Pos);
         begin
            if Subj.Name = Name then
               return Subj.Id;
            end if;
         end;
         Subjects_Package.Next (Position => S_Pos);
      end loop;

      return -1;
   end Get_Id;

end Skp;
