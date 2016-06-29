pragma Style_Checks (Off);

package Vt_Component.Channels
is

   Console_1_Address : constant := 16#0010_0000#;
   Console_1_Size    : constant := 16#0001_0000#;
   Console_1_Kind    : constant Channel_Kind := Channel_Reader;
   Console_1_Vector  : constant := 34;

   Console_2_Address : constant := 16#0011_0000#;
   Console_2_Size    : constant := 16#0001_0000#;
   Console_2_Kind    : constant Channel_Kind := Channel_Reader;
   Console_2_Vector  : constant := 35;

   Input_Device_1_Address : constant := 16#0005_0000#;
   Input_Device_1_Size    : constant := 16#1000#;
   Input_Device_1_Kind    : constant Channel_Kind := Channel_Writer;
   Input_Device_1_Event   : constant := 1;

   Input_Device_2_Address : constant := 16#0005_1000#;
   Input_Device_2_Size    : constant := 16#1000#;
   Input_Device_2_Kind    : constant Channel_Kind := Channel_Writer;
   Input_Device_2_Event   : constant := 2;

   Debuglog_Address : constant := 16#000f_fff0_0000#;
   Debuglog_Size    : constant := 16#0001_0000#;
   Debuglog_Kind    : constant Channel_Kind := Channel_Writer;

end Vt_Component.Channels;
