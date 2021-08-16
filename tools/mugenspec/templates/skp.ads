--  Disable line length check
pragma Style_Checks ("-m");

--D @Interface
--D The \texttt{Skp} package hierarchy is a codified static representation of
--D the system policy. All the values are derived from the system policy and
--D parameterize the Muen SK on the source level.
--D \paragraph*{}
--D This package contains numeric constants and range type definitions
--D derived from the system policy.
package Skp
is

   CPU_Count : constant := __cpu_count__;

   type CPU_Range is range 0 .. CPU_Count - 1;

   subtype APIC_ID_Type is Natural
     with Static_Predicate => APIC_ID_Type in __valid_apic_ids__;

   CPU_To_APIC_ID : constant array (CPU_Range) of APIC_ID_Type := (
__cpu_to_apic_id__);

   subtype Global_Subject_ID_Type is Natural range __subj_range__;

   Invalid_Subject : constant := Global_Subject_ID_Type'Last + 1;

   subtype Dst_Subject_Type is Natural range 0 .. Invalid_Subject;

   type Vector_Range is range 0 .. 255;

   Invalid_Vector : constant := 256;

   type Dst_Vector_Range is range 0 .. Invalid_Vector;

   Vmxon_Address : constant := __vmxon_addr__;

end Skp;
