pragma Style_Checks (Off);

package Vt_Component.Memory_Arrays
is

   Memarray_Address_Base  : constant := 16#5000#;
   Memarray_Element_Size  : constant := 16#1000#;
   Memarray_Element_Count : constant := 2;
   Memarray_Executable    : constant Boolean := True;
   Memarray_Writable      : constant Boolean := False;

   Memarray_Names : constant Name_Array (1 .. Memarray_Element_Count)
     := (
         1 => To_Name (Str => "mem1"),
         2 => To_Name (Str => "mem2")
        );

end Vt_Component.Memory_Arrays;
