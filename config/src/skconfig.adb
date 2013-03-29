with Ada.Text_IO;
with Ada.Strings.Unbounded;

with SK.Utils;

with Skp.Binaries;

with Skc.Subjects;

procedure Skconfig
is
   use Ada.Strings.Unbounded;

   Top_Dir : constant String := "..";
   Bins    : Skc.Subjects.Binary_Array;
begin
   for B in Bins'Range loop
      declare
         Subj : constant String := Top_Dir & "/" & To_String
           (Skp.Binaries.Binary_Specs (B).Path);
      begin
         Ada.Text_IO.Put_Line (Item => "Subject '" & Subj & "'");

         Bins (B) := Skc.Subjects.Read (Binary => Subj);

         Ada.Text_IO.Put_Line (Item => "  Entry " & SK.Utils.To_Hex
                               (Item => Bins (B).Entry_Point));
         Ada.Text_IO.Put_Line (Item => "  Stack " & SK.Utils.To_Hex
                               (Item => Bins (B).Stack_Address));
      end;
   end loop;

   Skc.Subjects.Write (Spec  => "include/skc-subjects.ads",
                       Subjs => Bins);
end Skconfig;
