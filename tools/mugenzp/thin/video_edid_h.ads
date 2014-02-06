with Interfaces.C; use Interfaces.C;

package video_edid_h is

   type edid_info_dummy_array is array (0 .. 127) of aliased unsigned_char;
   type edid_info is record
      dummy : aliased edid_info_dummy_array;  -- /usr/include/video/edid.h:5
   end record;
   pragma Convention (C_Pass_By_Copy, edid_info);  -- /usr/include/video/edid.h:4

end video_edid_h;
