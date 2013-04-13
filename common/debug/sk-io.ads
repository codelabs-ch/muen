--# inherit
--#    X86_64,
--#    SK;
package SK.IO
is

   --  Receive byte from given port.
   procedure Inb
     (Port  :     SK.Word16;
      Value : out SK.Byte);
   --# global
   --#    in X86_64.State;
   --# derives
   --#    Value from Port, X86_64.State;

   --  Send byte to given port.
   procedure Outb
     (Port  : SK.Word16;
      Value : SK.Byte);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Port, Value;

end SK.IO;
