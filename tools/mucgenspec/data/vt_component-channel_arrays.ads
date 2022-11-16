pragma Style_Checks (Off);

package Vt_Component.Channel_Arrays
is

   Input_Arr_Address_Base  : constant := 16#0001_0000#;
   Input_Arr_Element_Size  : constant := 16#1000#;
   Input_Arr_Element_Count : constant := 2;
   Input_Arr_Element_Kind  : constant Channel_Kind := Channel_Reader;
   Input_Arr_Vector_Base   : constant := 32;

   Input_Arr_Names : constant Name_Array (1 .. Input_Arr_Element_Count)
     := (
         1 => To_Name (Str => "input1"),
         2 => To_Name (Str => "input2")
        );

   Output_Arr_Address_Base  : constant := 16#0002_0000#;
   Output_Arr_Element_Size  : constant := 16#2000#;
   Output_Arr_Element_Count : constant := 3;
   Output_Arr_Element_Kind  : constant Channel_Kind := Channel_Writer;
   Output_Arr_Event_Base    : constant := 16;

   Output_Arr_Names : constant Name_Array (1 .. Output_Arr_Element_Count)
     := (
         1 => To_Name (Str => "output1"),
         2 => To_Name (Str => "output2"),
         3 => To_Name (Str => "output3")
        );

end Vt_Component.Channel_Arrays;
