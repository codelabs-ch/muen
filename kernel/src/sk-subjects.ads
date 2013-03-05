with SK.CPU;

--# inherit
--#    SK.CPU;
package SK.Subjects
--# own
--#    Descriptors;
--# initializes
--#    Descriptors;
is

   pragma Elaborate_Body (Subjects);

   --  Subject state.
   type Subject_State_Type is record
      Regs        : CPU.Registers_Type;
      Entry_Point : SK.Word64;
   end record;

   type Subject_Idx_Type is mod 2 ** 1;

   type Subject_Array is array (Subject_Idx_Type) of Subject_State_Type;

   --  Descriptors used to manage subjects.
   Descriptors : Subject_Array;

end SK.Subjects;
