--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ahven.Framework;

package Map_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Independent empty regions, inserted in increasing order
   procedure Non_Overlapping_Sorted;

   --  Independent empty regions, inserted in random order
   procedure Non_Overlapping_Random;

   --  Independent empty regions, inserted in decreasing order
   procedure Non_Overlapping_Reversed;

   --  Detect overlapping empty regions, overlapping left
   --      [existing region]
   --  [new region]
   procedure Overlapping_Empty_Left;

   --  Detect overlapping empty regions, overlapping right
   --      [existing region]
   --                  [new region]
   procedure Overlapping_Empty_Right;

   --  Detect overlapping empty regions, included
   --      [existing region]
   --         [new region]
   procedure Overlapping_Empty_Included;

   --  Detect overlapping empty regions, encompassing
   --      [existing region]
   --    [.....new region.....]
   procedure Overlapping_Empty_Encompassing;

   --  Test if consecutive regions are merged into one, inserted in random
   --  order
   procedure Region_Merge_Random;

   --  Test if consecutive regions are merged into one, inserted in decreasing
   --  order
   procedure Region_Merge_Reversed;

   --  Test if consecutive regions are merged into one, inserted in increasing
   --  order
   procedure Region_Merge_Sorted;

   --  Allocate a fixed region (i.e. start and end address is known) inside the
   --  region map that equals one complete empty in that map
   procedure Allocate_Fixed_Full_Empty_Region;

   --  Allocate a fixed region that spans the left part of an empty region
   procedure Allocate_Fixed_Partial_Left;

   --  Allocate a fixed region that spans the right part of an empty region
   procedure Allocate_Fixed_Partial_Right;

   --  Allocate a fixed region that spans the middle part of an empty region
   procedure Allocate_Fixed_Partial_Middle;

   --  Try to allocate a region that lies completely outside empty regions
   procedure Allocate_Fixed_Invalid_Outside_Empty;

   --  Try to allocate a region that lies outside empty regions on the left
   procedure Allocate_Fixed_Invalid_Left;

   --  Try to allocate a region that lies outside empty regions on the right
   procedure Allocate_Fixed_Invalid_Right;

   --  Try to allocate a region that exceeds the empty region
   procedure Allocate_Fixed_Invalid_Exceed;

   --  Try to allocate a region that spans multiple empty regions (empty
   --  regions are always non-continous, as adjacent regions are merged when
   --  adding them!)
   procedure Allocate_Fixed_Invalid_Multiple;

   --  Try to allocate a region inside an aleady allocated region
   procedure Allocate_Fixed_Invalid_Double;

   --  Try to allocate a region partially inside an aleady allocated region
   procedure Allocate_Fixed_Invalid_Partial_Double;

   --  Allocate a region that has a special alignment
   procedure Allocate_Variable_Aligned;

   --  Allocate a region based on size and alignment, such that an empty
   --  region is used completely
   procedure Allocate_Variable_Exact;

   --  Try to allocate a region that is too large
   procedure Allocate_Variable_OOM;

   --  Try to allocate a region which does not fit into empty memory due to
   --  fragmentation
   procedure Allocate_Variable_OOM_Fragmentation;

   --  Try to allocate a region which does not fit into empty memory due to
   --  alginment constraints
   procedure Allocate_Variable_OOM_Alignment;

end Map_Tests;
