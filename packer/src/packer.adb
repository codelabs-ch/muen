with Ada.Text_IO;

with Pack.Image;

procedure Packer
is
   use Pack;
begin
   Ada.Text_IO.Put_Line ("Packaging kernel image ...");

   --  Kernel sections.

   Image.Add_Section (Filename => "../policy/include/kernel_pt",
                      Address  => 16#00200000#);

   --  Subjects.

   Image.Add_Section (Filename => "obj/tau0",
                      Address  => 16#00240000#);
   Image.Add_Section (Filename => "../policy/include/tau0_pt",
                      Address  => 16#001f0000#);
   Image.Add_Section (Filename => "../policy/include/tau0_iobm",
                      Address  => 16#00244000#);

   Image.Add_Section (Filename => "obj/subject_1",
                      Address  => 16#00250000#);
   Image.Add_Section (Filename => "../policy/include/subject1_pt",
                      Address  => 16#001f4000#);
   Image.Add_Section (Filename => "../policy/include/subject1_iobm",
                      Address  => 16#00254000#);

   Image.Add_Section (Filename => "obj/subject_2",
                      Address  => 16#00260000#);
   Image.Add_Section (Filename => "../policy/include/subject2_pt",
                      Address  => 16#001f8000#);
   Image.Add_Section (Filename => "../policy/include/subject2_iobm",
                      Address  => 16#00264000#);
end Packer;
