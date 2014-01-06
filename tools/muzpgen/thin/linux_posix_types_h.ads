with Interfaces.C; use Interfaces.C;

package linux_posix_types_h is

   type uu_kernel_fd_set_fds_bits_array is array (0 .. 31) of aliased unsigned_long;
   type uu_kernel_fd_set is record
      fds_bits : aliased uu_kernel_fd_set_fds_bits_array;  -- /usr/include/linux/posix_types.h:25
   end record;
   pragma Convention (C_Pass_By_Copy, uu_kernel_fd_set);  -- /usr/include/linux/posix_types.h:26

   --  skipped anonymous struct anon_0

   type uu_kernel_sighandler_t is access procedure (arg1 : int);  -- /usr/include/linux/posix_types.h:29

   subtype uu_kernel_key_t is int;  -- /usr/include/linux/posix_types.h:32

   subtype uu_kernel_mqd_t is int;  -- /usr/include/linux/posix_types.h:33

end linux_posix_types_h;
