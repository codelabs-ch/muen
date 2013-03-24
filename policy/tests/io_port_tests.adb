with SK;

with Skp.IO_Ports;

package body IO_Port_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "I/O port tests");
      T.Add_Test_Routine
        (Routine => IO_Bitmap_Handling'Access,
         Name    => "I/O bitmap handling");
   end Initialize;

   -------------------------------------------------------------------------

   procedure IO_Bitmap_Handling
   is
      use type SK.Word16;

      Port1 : constant SK.Word16 := 16#50b0#;
      Port2 : constant SK.Word16 := 16#03d4#;
      Port3 : constant SK.Word16 := 16#03d5#;
      B     : IO_Ports.IO_Bitmap_Type := IO_Ports.Null_IO_Bitmap;
   begin
      for P in SK.Word16 loop
         Assert (Condition => not IO_Ports.Is_Allowed
                 (B    => B,
                  Port => P),
                 Message   => "Null bitmap allows access");
      end loop;

      IO_Ports.Allow_Ports (B          => B,
                            Start_Port => Port1,
                            End_Port   => Port1);
      for P in SK.Word16 loop
         if P = Port1 then
            Assert (Condition => IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error allowing single port (1)");
         else
            Assert (Condition => not IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error allowing single port (2)");
         end if;
      end loop;

      IO_Ports.Allow_Ports (B          => B,
                            Start_Port => Port2,
                            End_Port   => Port3);
      for P in SK.Word16 loop
         if P = Port1 or P in Port2 .. Port3 then
            Assert (Condition => IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error allowing ports (1)");
         else
            Assert (Condition => not IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error allowing ports (2)");
         end if;
      end loop;

      IO_Ports.Deny_Ports (B          => B,
                           Start_Port => Port1,
                           End_Port   => Port1);
      for P in SK.Word16 loop
         if P in Port2 .. Port3 then
            Assert (Condition => IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error denying port (1)");
         else
            Assert (Condition => not IO_Ports.Is_Allowed
                    (B    => B,
                     Port => P),
                    Message   => "Error denying port (2)");
         end if;
      end loop;

      IO_Ports.Deny_Ports (B          => B,
                           Start_Port => Port2,
                           End_Port   => Port3);
      for P in SK.Word16 loop
         Assert (Condition => not IO_Ports.Is_Allowed
                 (B    => B,
                  Port => P),
                 Message   => "Error denying ports");
      end loop;
   end IO_Bitmap_Handling;

end IO_Port_Tests;
