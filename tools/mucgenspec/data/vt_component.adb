package body Vt_Component
is

   -------------------------------------------------------------------------

   function To_Name (Str : String) return Name_Type
   is
      N    : String (Name_Type'Range) := (others => ASCII.NUL);
      Last : constant Natural
        := Natural'Min (Str'Length, Name_Type'Last);
   begin
      for I in N'First .. Last loop
         N (I) := Str (Str'First + (I - 1));
      end loop;
      return Name_Type (N);
   end To_Name;

end Vt_Component;
