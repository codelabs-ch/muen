with Interfaces;

package NVMe
is

   subtype Queue_Range is Interfaces.Unsigned_32 range 2 .. 65_535; -- 0 .. ?

   subtype Admin_Queue_Range is Interfaces.Unsigned_32 range 2 .. 4_095;

   NVMe_Class_Code : constant := 16#010802#;

   type Status_Type is (OK, Timeout, Fail, Unknown);
   for Status_Type use (OK => 0, Timeout => 1, Fail => 2, Unknown => 3);

end NVMe;
