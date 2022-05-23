package Xt_Component
is

   type Channel_Kind is (Channel_Reader, Channel_Writer);

   type Name_Type is new String (1 .. 63);

   function To_Name (Str : String) return Name_Type;

   type Name_Array is array (Positive range <>) of Name_Type;

end Xt_Component;
