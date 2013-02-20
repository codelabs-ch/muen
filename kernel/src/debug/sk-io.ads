package SK.IO
is

   --  Receive byte from given port.
   procedure Inb
     (Port  :     Word16;
      Value : out Byte);

   --  Send byte to given port.
   procedure Outb
     (Port  : Word16;
      Value : Byte);

end SK.IO;
