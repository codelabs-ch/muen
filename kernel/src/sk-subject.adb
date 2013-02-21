with System.Storage_Elements;

with SK.CPU;
with SK.Console;
with SK.Console_VGA;

package body SK.Subject
is

   --# hide SK.Subject;

   package Text_IO is new SK.Console
     (Initialize      => Console_VGA.Init,
      Output_New_Line => Console_VGA.New_Line,
      Output_Char     => Console_VGA.Put_Char);

   -------------------------------------------------------------------------

   procedure Main
   is
   begin
      Text_IO.Init;
      Text_IO.Put_Line (Item => "Hello from guest world");
      CPU.Panic;
   end Main;

   -------------------------------------------------------------------------

   function Get_Main_Address return SK.Word64
   is
   begin
      return SK.Word64
        (System.Storage_Elements.To_Integer
           (Value => Main'Address));
   end Get_Main_Address;

end SK.Subject;
