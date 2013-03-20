with Ada.Text_IO;
with Ada.Command_Line;

with SK.Utils;

with Skc.Subjects;

procedure Skconfig
is
   Count : constant Natural := Ada.Command_Line.Argument_Count;
   Bins  : Skc.Subjects.Binary_Array (1 .. Count);
begin
   for B in Bins'Range loop
      declare
         Subj : constant String := Ada.Command_Line.Argument (B);
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
