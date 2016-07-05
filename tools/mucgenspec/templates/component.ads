package __component_name___Component
is

   type Name_Type is new String (1 .. 63);

   function To_Name (Str : String) return Name_Type;

   type Name_Array is array (Positive range <>) of Name_Type;

end __component_name___Component;
