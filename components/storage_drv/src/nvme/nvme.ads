-- All Spec references reference NVMe Base Spec 2.0c
package NVMe
is

   NVMe_Class_Code : constant := 16#010802#;

   type Status_Type is (OK, Timeout, Fail, Unknown);
   for Status_Type use (OK => 0, Timeout => 1, Fail => 2, Unknown => 3);

end NVMe;
