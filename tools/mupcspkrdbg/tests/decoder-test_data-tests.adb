--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Decoder.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Decoder.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Decode (Gnattest_T : in out Test);
   procedure Test_Decode_7b4784 (Gnattest_T : in out Test) renames Test_Decode;
--  id:2.2/7b4784d82b59bab0/Decode/1/0/
   procedure Test_Decode (Gnattest_T : in out Test) is
   --  decoder.ads:27:4:Decode
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Directories;
      use Ada.Text_IO;
      
      CR : constant Character := Character'Val (13);

      Input_Filename  : constant String := "data/testcase.txt";
      Output_Filename : constant String := "obj/decoded.txt";

      Input, Output : File_Type;
      
      Old_Output : constant File_Type := Standard_Output;
   begin
      Open (File => Input, Mode => In_File, Name => Input_Filename);
      Create (File => Output, Mode => Out_File, Name => Output_Filename);
      
      Set_Input (File => Input);
      Set_Output (File => Output);
      
      Decode;

      Set_Output (File => Old_Output);
      Close (File => Output);
      Open (File => Output, Mode => In_File, Name => Output_Filename);

      Assert (Condition => Get_Line (File => Output) = "DBG-LOG>" & CR and then
                Get_Line (File => Output) = "16#0012#|SM subject running" & CR,
              Message   => "Decoding error");
      
      Close (File => Output);
      Close (File => Input);

      Delete_File (Name => Output_Filename);
      
   exception
      when others =>
         Close (File => Output);
         Close (File => Input);
         Delete_File (Name => Output_Filename);
         raise;
--  begin read only
   end Test_Decode;
--  end read only


--  begin read only
   procedure Test_Next_Frequency (Gnattest_T : in out Test);
   procedure Test_Next_Frequency_88f9f3 (Gnattest_T : in out Test) renames Test_Next_Frequency;
--  id:2.2/88f9f3df3cba5b0a/Next_Frequency/1/0/
   procedure Test_Next_Frequency (Gnattest_T : in out Test) is
   --  decoder.ads:45:4:Next_Frequency
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Text_IO;
      use Ada.Directories;

      Input_Filename : constant String := "data/testcase.txt";

      Input : File_Type;
   begin
      Open (File => Input,
            Mode => In_File,
            Name => Input_Filename);
      
      Set_Input (File => Input);
      
      Assert (Condition => Next_Frequency = 0.0,
              Message   => "Wrong frequency (1)");

      Assert (Condition => Next_Frequency = 200.805954,
              Message   => "Wrong frequency (2)");

      Close (File => Input);
      
   exception
      when others =>
         Close (File => Input);
         raise;
--  begin read only
   end Test_Next_Frequency;
--  end read only


--  begin read only
   procedure Test_Is_Byte_Sep (Gnattest_T : in out Test);
   procedure Test_Is_Byte_Sep_d147f2 (Gnattest_T : in out Test) renames Test_Is_Byte_Sep;
--  id:2.2/d147f2d97b24e95b/Is_Byte_Sep/1/0/
   procedure Test_Is_Byte_Sep (Gnattest_T : in out Test) is
   --  decoder.ads:47:4:Is_Byte_Sep
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Byte_Sep (Frequency => Byte_Sep),
              Message   => "Classification error (1)");

      Assert (Condition => not Is_Byte_Sep (Frequency => Code (0, 0)),
              Message   => "Classification error (2)");

      Assert (Condition => not Is_Byte_Sep (Frequency => Code (1, 0)),
              Message   => "Classification error (3)");
      
      Assert (Condition => not Is_Byte_Sep (Frequency => Code (0, 1)),
              Message   => "Classification error (3)");

      Assert (Condition => not Is_Byte_Sep (Frequency => Code (1, 1)),
              Message   => "Classification error (4)");
--  begin read only
   end Test_Is_Byte_Sep;
--  end read only


--  begin read only
   procedure Test_Is_Nearly (Gnattest_T : in out Test);
   procedure Test_Is_Nearly_d0f5b1 (Gnattest_T : in out Test) renames Test_Is_Nearly;
--  id:2.2/d0f5b150d681e91b/Is_Nearly/1/0/
   procedure Test_Is_Nearly (Gnattest_T : in out Test) is
   --  decoder.ads:49:4:Is_Nearly
--  end read only

      pragma Unreferenced (Gnattest_T);
      
      function Pair_Distinct
        (P1, P2 : Phase_Type;
         B1, B2 : Bit_Type)
         return Boolean
      is
         use type Interfaces.Unsigned_8;

         E1 : constant Boolean
           := (not Is_Nearly (Code (P1, B1), Code (P2, B2)))
              or (P1 = P2 and B1 = B2);
         E2 : constant Boolean
           := (not (P1 = P2 and B1 = B2))
              or Is_Nearly (Code (P1, B1), Code (P2, B2));
      begin
         return E1 and E2;
      end Pair_Distinct;
   begin
      Assert (Condition => Is_Nearly (487.0, 500.0),
              Message   => "Classification error (1)");

      Assert (Condition => not Is_Nearly (300.0, 500.0),
              Message   => "Classification error (2)");
      
      for P1 in Phase_Type loop
         for B1 in Bit_Type loop
            for P2 in Phase_Type loop
               for B2 in Bit_Type loop
                  Assert (Condition => Pair_Distinct (P1, P2, B1, B2),
                          Message   => "Non-distinct codes. P1 = "
                            & P1'Img & ", P2 = " & P2'Img & ", B1 = " & B1'Img
                            & ", B2 = " & B2'Img);
               end loop;
            end loop;
         end loop;
      end loop;
--  begin read only
   end Test_Is_Nearly;
--  end read only


--  begin read only
   procedure Test_Symbol_From_Freq (Gnattest_T : in out Test);
   procedure Test_Symbol_From_Freq_bfab97 (Gnattest_T : in out Test) renames Test_Symbol_From_Freq;
--  id:2.2/bfab9736b4ff8e09/Symbol_From_Freq/1/0/
   procedure Test_Symbol_From_Freq (Gnattest_T : in out Test) is
   --  decoder.ads:51:4:Symbol_From_Freq
--  end read only

      pragma Unreferenced (Gnattest_T);
      
      use type Interfaces.Unsigned_8;

      Found : Boolean;
      Phase : Phase_Type;
      Bit   : Bit_Type;
   begin
      Symbol_From_Freq (Frequency => Code (0, 0),
                        Found     => Found,
                        Phase     => Phase,
                        Bit       => Bit);

      Assert (Condition => Found and Phase = 0 and Bit = 0,
              Message   => "Classification error (1)");

      Symbol_From_Freq (Frequency => Byte_Sep,
                        Found     => Found,
                        Phase     => Phase,
                        Bit       => Bit);

      Assert (Condition => not Found,
              Message   => "Classification error (2)");
--  begin read only
   end Test_Symbol_From_Freq;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Decoder.Test_Data.Tests;
