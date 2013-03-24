------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements state machines (non-deterministic, aka NFA, and
--  deterministic, aka DFA).

pragma Ada_05;

with GNAT.Dynamic_Tables;

generic
   type Symbol is private;
   --  The symbols accepted by the state machine as input.

   type Transition_Symbol is private;
   --  One such symbol might be associated with each transition (although this
   --  is optional) to speed up the processing of the state machine

   with function Image (Sym : Transition_Symbol) return String;
   --  Display Sym.

   type State_User_Data is private;
   Default_Data : State_User_Data;
   --  User data associated with each state

   Default_State_Count      : Positive := 100;
   Default_Transition_Count : Positive := 100;
   --  Default size of the state machine initially

package Sax.State_Machines is

   type State is new Natural; --   range 0 .. 2 ** 16 - 1;
   --  A state of a state machine

   type State_Data_Access is access all State_User_Data;

   -----------------------------------------------
   -- Non-deterministic automatons construction --
   -----------------------------------------------

   type NFA is tagged private;
   type NFA_Access is access all NFA'Class;
   --  A non-deterministic automaton

   procedure Initialize
     (Self                 : in out NFA;
      States_Are_Statefull : Boolean := False);
   --  Initializes a new automaton
   --  If [States_Are_Statefull], the active states's user data will be used to
   --  perform various things. Otherwise, the exact list of states are
   --  irrelevant, and we are only interested in the transitions between them.
   --  This setting affects the way the machine is created in the call to
   --  [Repeat]. When the setting is True, more empty transitions will have to
   --  be created. For instance:
   --     if the transition "a" is to be repeated 0 or 1 time, the state
   --     machine will be:
   --           [1]---a---->[2]  if not States_Are_Statefull
   --              \-------/
   --    Or
   --           [1]---a---->[2]---->[3]   if States_Are_Statefull
   --              \----------------/
   --
   --  So when processing the events, the state [2] might be active initially
   --  in one of the cases.

   procedure Free (Self : in out NFA);
   procedure Free (Automaton : in out NFA_Access);
   --  Free the memory allocated to [Self]

   type State_Callback is access procedure
     (Machine   : access NFA'Class;
      For_State : State);
   --  A callback when some event occurs on a state

   function Add_State
     (Self     : access NFA;
      Data     : State_User_Data := Default_Data) return State;
   --  Add a new state into the table.

   No_State    : constant State;
   Start_State : constant State;
   Final_State : constant State;
   --  The start and final states of an automation
   --  There is a single one of each per automaton, but you can of course
   --  connect them, through empty transitions, to any number of states within
   --  [Self], thus making them start states in effect.
   --  These two states always exist.

   procedure Set_Data (Self : access NFA; S : State; Data : State_User_Data);
   function Get_Data (Self : access NFA; S : State) return State_Data_Access;
   --  Returns an access to the state's user data. This can be modified in
   --  place, but the access type should not be stored since it still belongs
   --  to the NFA.
   --  This API is slightly faster and more convenient than having a
   --  [Get_User_Data] and [Set_User_Data] set of subprograms.

   procedure Add_Transition
     (Self       : access NFA;
      From       : State;
      To         : State;
      On_Symbol  : Transition_Symbol);
   --  Add a new transition between the two states.
   --  If the symbol given as input to [Self] matches On_Symbol, a transition
   --  will occur from [From_State] to [To_State].
   --  Both states might be equal.
   --  You cannot add transitions from the final state

   procedure Add_Empty_Transition
     (Self : access NFA;
      From : State;
      To   : State);
   --  Indicates that any time [Self] is in [From_State], it should also be
   --  considered as in [To_State]. Both states are basically equivalent.
   --  You cannot add transitions from the final state

   function Repeat
     (Self       : access NFA;
      From, To   : State;
      Min_Occurs : Natural := 1;
      Max_Occurs : Natural := 1) return State;
   --  Modify the automaton to repat the subautomaton From_State .. To_State a
   --  specific number of times.
   --  Note that this requires expansion (for instance "e{3,4}" is expanded to
   --  "eeee?", so requires more states), so Max_Occurs should not be too big.
   --
   --  Here is an example of use (equivalent to 'b{2,3}' in traditional regexp)
   --     A := N.Add_State;
   --     B := N.Add_State;
   --     N.Add_Transition (A, B, 'b');
   --     C := N.Repeat (A, B, 2, 3);
   --
   --  On exit, [From] and [To] are still the original sub-automaton. The
   --  returned value is the end state of the repeated automaton (ie it plays
   --  the same role as [To] in the original NFA.
   --  You would connect to the returned state if you have further states to
   --  add. You should not, however, directly connect from or to any state
   --  within [From]..[To] (since they might have been duplicated).
   --
   --  No error is reported if Min_Occurs > Max_Occurs. But nothing is done
   --  either.

   ---------------
   -- Snapshots --
   ---------------
   --  A snapshot saves the states and transitions of the machine, and can be
   --  used to reset the NFA so that all states and transitions added after
   --  that point are removed.

   type NFA_Snapshot is private;
   No_NFA_Snapshot : constant NFA_Snapshot;

   function Get_Snapshot (Self : access NFA) return NFA_Snapshot;
   procedure Reset_To_Snapshot (Self : access NFA; Snapshot : NFA_Snapshot);
   --  Saves the list of states and transitions, so that we can later delete
   --  all states and transitions added after that point.

   function Exists (Snapshot : NFA_Snapshot; S : State) return Boolean;
   --  Whether [S] existed in [Snapshot]

   ----------------------------------------
   -- Hierarchical finite state machines --
   ----------------------------------------
   --  It is possible to build hierarchical state machines: in such machines,
   --  some of the states will contain nested state machines.
   --  For instance:
   --                +----2-----+
   --                |          |---'b'--> 7
   --      1 ------->|-4->5-->6----------> 3
   --                |          |
   --                +----------+
   --
   --  In the case above, the machine could be in both state 2 (the superstate)
   --  and in state 5 (the inner state).
   --  When an input is processed, all active states (super and inner) will
   --  proceed the event. If 5 matches, we might go to 6. Next time, if 6
   --  matches, we would exit 2 and go to 3.
   --
   --  But when 2 and 5 are active, it is also possible that 2 itself matches,
   --  and then we go to 3 whatever inner state we were in at the same time.
   --  This is the usual behavior (as defined for instance in UML).
   --
   --  The above would be created as follows. Note that this example also does
   --  not assume that the nested NFA has been created before we create the
   --  toplevel NFA.
   --
   --     S1 := N.Add_State; S2 := N.Add_State; S3 := N.Add_State;
   --     N.Add_Transition (S1, S2, ...);  --  will enter "2" and nested NFA
   --                                      --  so activate S4
   --     N.Add_Transition (S2, S7, 'b');  --  will exit nested NFA whatever
   --                                      --  state we are in.
   --
   --  Later on we create the nested automaton:
   --     S4 := N.Add_State;
   --     S5 := N.Add_State;  N.Add_Transition (S4, S5, ...);
   --     S6 := N.Add_State;  N.Add_Transition (S5, S6, ...);
   --
   --     E := N.Create_Nested (S4);
   --     N.Set_Nested (E);  --  Wraps E (we could have several states wrapping
   --                        --  the same nested)
   --     N.On_Nested_Exit (S2, S3);  --  on exit of nested NFA, moves to 3
   --
   --  It is possible to build state machines that cannot be executed later on:
   --  if the state machine within 2 is the same as the outer state machine
   --  (therefore we have a recursive state machine, somewhat), and we have an
   --  empty transition from 1 to 4, then the initial state would require
   --  infinite storage for the NFA_Matcher: we start in state 1, which through
   --  the empty transition is the same as state 4 (and therefore state 2 is
   --  also active). But in this recursive NFA, state 4 is another instance
   --  equivalent to state 1. In turn, we have another nested state 1, then
   --  another,... ad infinitum. So you should always have a non-empty
   --  transition into a nested state machine (in thhe schema above, transition
   --  from 1->4 should not be empty.
   --
   --  It is invalid to add a transition from one of the nested states from one
   --  of the outer states. The nested automaton must be fully independent,
   --  since it might be reused in several places.

   type Nested_NFA is private;
   No_Nested : constant Nested_NFA;

   function Create_Nested
     (Self : access NFA'Class; From : State) return Nested_NFA;
   --  Marks the part of the machine starting at [From] and ending at [To] as
   --  a nested automaton. It is possible that some states have been created
   --  between the two that do not belong to the nested automaton, this isn't
   --  an issue.
   --  The state [From] is the default state for the nested automaton. For
   --  instance, a camera has two superstates: "on" and "off". The "on" state
   --  has a nested NFA for "record" and "playback" modes. By default, if you
   --  enter the "on" state, the "record" mode is also selected. However, using
   --  the appropriate camera button, it is possible to enter the "playback"
   --  button directly.
   --
   --  Within the nested NFA, transitions to [Final_State] play a special role:
   --  upon reaching it, the nested automaton will be terminated, and control
   --  returned to the super state (that state is one of the states for which
   --  we have called Set_Nested). Any empty transition for the superstate will
   --  be navigated to find out the new list of active states.
   --
   --  No further internal transition must be added to the nested automaton
   --  after this call, since its states have been marked specially. It is
   --  still valid to add transitions to the ouside.

   procedure Set_Nested
     (Self            : access NFA;
      S               : State;
      Nested          : Nested_NFA);
   function Get_Nested (Self : access NFA; S : State) return Nested_NFA;
   --  Setup state [S] so that it includes a nested NFA defined by [Nested]

   function Get_Start_State (Self : Nested_NFA) return State;
   --  Return the start state that was defined for the nested NFA

   procedure On_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State;
      On_Symbol : Transition_Symbol);
   procedure On_Empty_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State);
   --  When the nested NFA in [From] is terminated (because it has reached
   --  [Final_State] after processing [On_Symbol]), a transition from [From] to
   --  [To] is performed. [Set_Nested] must have been called for [From] first.

   -------------------------
   -- Dumping information --
   -------------------------
   --  The following subprograms are used mostly for debugging, and can be used
   --  to visualize the contents of a state machine, either textually or
   --  graphically

   function Default_Image
     (Self : access NFA'Class; S : State; Data : State_User_Data)
      return String;
   --  The default display for states (only displays the state number)

   type Dump_Mode is
     (Dump_Multiline, Dump_Compact, Dump_Dot, Dump_Dot_Compact);
   --  The type of dump we can do for the graph:
   --  [Dump_Multiline]: Each state is displayed on one line
   --  [Dump_Compact]:   Output is on a single line
   --  [Dump_Dot):       Output that can be cut-and-pasted to use by the
   --                    graphviz suite to display a graphical representation

   generic
      with function State_Image
        (Self : access NFA'Class;
         S    : State;
         Data : State_User_Data) return String is Default_Image;
      --  This function is never called for the final state, which has no
      --  user data associated with it. Nor it is called for the start state.

   package Pretty_Printers is
      --  This package provides various functions to view the current state of
      --  a NFA. It is generic so that most users who instantiate a State
      --  Machine do not have to provide a State_Image function.

      function Dump
        (Self                : access NFA'Class;
         Mode                : Dump_Mode := Dump_Compact;
         Show_Details        : Boolean := True;
         Show_Isolated_Nodes : Boolean := True;
         Since               : NFA_Snapshot := No_NFA_Snapshot) return String;
      --  Dump the NFA into a string.
      --  This is mostly for debug reasons, and the output might change from
      --  one version to the next.
      --  If [Compact] is True then the output does not include newlines.
      --  If [Show_Details] is False, then only the count of nodes will be
      --  displayed, not the actual list of nodes and transitions.
      --  If [Show_Isolated_Nodes] is false, then nodes that are not linked
      --  to any other and have no nested node will not be displayed.
      --  Only states greater than [Since] are displayed: for instance,
      --  if you already have a NFA to start with and you are adding to it, you
      --  can view just your addition using this parameter.

      function Dump
        (Self   : access NFA'Class;
         Nested : Nested_NFA;
         Mode   : Dump_Mode := Dump_Compact) return String;
      --  Dump the NFA into a string.

      function Node_Label
        (Self : access NFA'Class; S : State) return String;
      --  Textual representation of a state, based on State_Image.

   end Pretty_Printers;

   -------------------------------------------
   -- Non-deterministic automatons matching --
   -------------------------------------------

   type Abstract_NFA_Matcher is abstract tagged null record;

   generic
      type Active_State_Data is private;
      No_Active_Data : Active_State_Data;
      --  Extra data associated with each active state. This data will be
      --  copied from one iteration of the matcher to the next even if the
      --  state remains active, so you should use small data and not
      --  memory-allocated types.

      with function Match
        (Self       : access Abstract_NFA_Matcher'Class;
         From_State, To_State : State;
         Parent_State_Data : access Active_State_Data;
         Trans      : Transition_Symbol;
         Input      : Symbol) return Boolean;
      --  Whether the two symbols match. In particular this means that the
      --  corresponding transition is valid.
      --  Using the "=" operator might be enough in a lot of cases, but will
      --  not handle the case where the transitions are more general (for
      --  instance, allowing a transition on integers where the symbol is
      --  between 1 and 10).
      --  The NFA associated with Self must not be modified by this function,
      --  since it might be shared among several matchers. Self, on the other
      --  hand, can be freely modified.
      --  This function can also be used to implement conditional transitions:
      --  if you store the condition as part of the Transition_Symbol, you can
      --  then evaluate it as part of this function. This function, however,
      --  is never called for empty transitions, so these cannot be made
      --  conditional.

      with function Expected
        (Self                 : Abstract_NFA_Matcher'Class;
         From_State, To_State : State;
         Parent_State_Data    : access Active_State_Data;
         Trans                : Transition_Symbol) return String;
      --  This function should return the name to display in the result of
      --  Expected to show what transitions are expected. It is only called
      --  when Trans.Kind is Transition_On_Symbol.
      --  The default implementation should be something like:
      --      return Image (Trans);
      --  It should return the empty string if the transition is not valid
      --  (when you implemented conditional transitions).

   package Matchers is
      --  This package contains the actual processor to process a series of
      --  input events, and compute at each step which are the active states
      --  in the automaton. This package is generic so that for a given NFA
      --  there can be several different ways to process it and intrepret the
      --  events.

      type NFA_Matcher is new Abstract_NFA_Matcher with private;
      --  When processing an input, the state machine is left untouched.
      --  Instead, the required information is stored in a separate object,
      --  so that multiple objects can test the same machine in parallel..
      --  It is valid to modify the state machine during the lifetime of a
      --  matcher. However, this will only affect the matcher the next time
      --  [Process] is called (so for instance adding an empty transition will
      --  never impact existing matchers.

      procedure Free (Self : in out NFA_Matcher);
      --  Free the memory allocated for [Self]

      function Is_Initialized (Self : NFA_Matcher) return Boolean;
      --  Whether the NFA has been initialized through a call to Start_Match
      --  (and not yet been freed through a call to Free)

      procedure Start_Match
        (Self     : in out NFA_Matcher;
         On       : access NFA'Class;
         Start_At : State := Start_State);
      --  Return a matcher which is in [On]'s initial states.
      --  The matcher holds a reference to [On], so is only valid while [On]
      --  is in the scope.
      --  This function automatically frees Self, releasing any previously used
      --  memory.

      type Active_State_Iterator (<>) is private;
      No_Active_State_Iterator : constant Active_State_Iterator;
      --  Intended use is:
      --  declare
      --     Iter : Active_State_Iterator := For_Each_Active_State (Matcher);
      --  begin
      --     loop
      --        S := Current (Matcher, Iter);
      --        exit when S = No_State;
      --        ...
      --        Next (Matcher, Iter);
      --     end loop;
      --  end;

      function For_Each_Active_State
        (Self              : NFA_Matcher;
         Ignore_If_Nested  : Boolean := False;
         Ignore_If_Default : Boolean := False) return Active_State_Iterator;
      procedure Next
        (Self : NFA_Matcher; Iter : in out Active_State_Iterator);
      function Current
        (Self : NFA_Matcher; Iter : Active_State_Iterator) return State;
      --  Iterates over all currently active states.
      --  If [Ignore_If_Nested] is true, the states with a nested NFA are not
      --  returned unless their nested NFA is in a final state (that's because
      --  we would be ignoring events on them otherwise).
      --  If [Ignore_If_Default] is true, the states for which no user data was
      --  set are never returned.
      --  [Current] returns [No_State] when there are no remaining active
      --  states. Note that a given state might have several corresponding
      --  active states because of nested NFA.

      function Has_Parent (Iter : Active_State_Iterator) return Boolean;
      function Parent
        (Iter : Active_State_Iterator) return Active_State_Iterator;
      --  Return the parent state of the current state.

      function Current_Data
        (Self : NFA_Matcher; Iter : Active_State_Iterator)
         return State_User_Data;
      --  Returns the user data either from the locally overridden data in the
      --  matcher, or from the NFA. See [Override_Data].

      procedure Replace_State
        (Self : in out NFA_Matcher;
         Iter : Active_State_Iterator;
         S    : State);
      --  Replace the state pointed to by [Iter].
      --  This is only rarely useful, but for instance is used when
      --  validating a XML schema to handle the xsi:type that can be used
      --  to override the current state.
      --  This also activates the state accessible from [S] through an empty
      --  transition.

      procedure Override_Data
        (Self : NFA_Matcher;
         Iter : Active_State_Iterator;
         Data : State_User_Data);
      --  Overridde the user data associated with the current state. This only
      --  impacts the matcher, so this data is lost as soon as the current
      --  state is no longer active. Same as [Replace_State], this is rarely
      --  useful.

      function In_Final (Self : NFA_Matcher) return Boolean;
      --  Whether [Self] is in the final step: if True, it means that all input
      --  processed so far matches the state machine. It is possible to keep
      --  submitting input

      procedure Process
        (Self    : in out NFA_Matcher;
         Input   : Symbol;
         Success : out Boolean);
      --  Processes one input symbol, and compute the transitions.
      --  [Success] is set to False if the input was invalid, and no transition
      --  could be found for it. In such a case, [Self] is left unmodified.
      --  If [Success] is set to True, a new set of active states was computed,
      --  and at least one state is active.
      --  The transitions (and thus the calls to Match) are processed in the
      --  order they were created in the NFA.

      function Expected (Self : NFA_Matcher) return String;
      --  Return a textual description of the valid input symbols from the
      --  current state. This should be used for error messages for instance.

      generic
         with function Node_Label
           (Self : access NFA'Class; S : State) return String;
         --  Should come from an instantiation of Pretty_Printers
      procedure Debug_Print
        (Self   : Matchers.NFA_Matcher'Class;
         Mode   : Dump_Mode := Dump_Multiline;
         Prefix : String := "");
      --  Print on stdout some debug information for [Self].
      --  [Prefix] is printed at the beginning of the first line

   private
      type Matcher_State_Index is new Natural range 0 .. 2 ** 16;
      No_Matcher_State : constant Matcher_State_Index := 0;

      type Matcher_State is record
         S      : State;

         Data_Is_Overridden : Boolean         := False;
         Overridden_Data    : State_User_Data := Default_Data;

         Next   : Matcher_State_Index;
         Nested : Matcher_State_Index;

         Active_Data : aliased Active_State_Data;
      end record;
      --  All currently active states in a NFA.
      --  For each state, we store a pointer to the next state at the same
      --  level of the hierarchy (and within the same parent).
      --  It also stores a pointer to the list of nested states, if there is a
      --  nested state machine.
      --  If the state machine is in the final state at any level,
      --  [Final_State] will be the first element of the corresponding list.

      package Matcher_State_Arrays is new GNAT.Dynamic_Tables
        (Table_Component_Type => Matcher_State,
         Table_Index_Type     => Matcher_State_Index,
         Table_Low_Bound      => No_Matcher_State + 1,
         Table_Initial        => 15,
         Table_Increment      => 10);

      type Matcher_State_Array
        is array (Matcher_State_Index range <>) of Matcher_State_Index;
      --  Each element in the array is the currently active state at that
      --  level. so Arr(2) is nested in Arr(1),...

      type Active_State_Iterator (Max : Matcher_State_Index) is record
         Ignore_If_Nested  : Boolean;
         Ignore_If_Default : Boolean;
         States            : Matcher_State_Array (1 .. Max);
         Current_Level     : Matcher_State_Index := No_Matcher_State;
      end record;
      No_Active_State_Iterator : constant Active_State_Iterator :=
        (0, False, False, (1 .. 0 => No_Matcher_State), No_Matcher_State);

      type NFA_Matcher is new Abstract_NFA_Matcher with record
         NFA          : NFA_Access;
         Active       : Matcher_State_Arrays.Instance;
         First_Active : Matcher_State_Index := No_Matcher_State;
      end record;
      --  [First_Active] is the first active state at the toplevel.
   end Matchers;

private
   type Transition_Id is new State;

   type Transition_Kind is (Transition_On_Empty,
                            Transition_On_Symbol,
                            Transition_On_Exit_Empty,
                            Transition_On_Exit_Symbol);

   type Transition (Kind : Transition_Kind := Transition_On_Empty) is record
      To_State       : State;
      --  State the transition is pointing to.

      Next_For_State : Transition_Id;
      --  Next transition from the same state. This implements a list of
      --  transitions.

      case Kind is
         when Transition_On_Empty | Transition_On_Exit_Empty => null;
         when others => Sym : Transition_Symbol;
      end case;
   end record;
   No_Transition : constant Transition_Id := 0;

   Start_State : constant State := 1;           --  Exists in NFA.States
   Final_State : constant State := State'Last;  --  Not shown in NFA.States
   No_State    : constant State := 0;

   type State_Data is record
      First_Transition : Transition_Id;
      --  The first element in the list of transitions from this state.

      Nested           : State := No_State;
      --  If defined, indicates that this state contains a nested state
      --  machine, for which the initial state is Nested. Any transition
      --  to this state will also activate [Nested].

      Data             : aliased State_User_Data;
      --  Custom data associated with each state.
   end record;

   package Transition_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Transition,
      Table_Index_Type     => Transition_Id,
      Table_Low_Bound      => No_Transition + 1,
      Table_Initial        => Default_Transition_Count,
      Table_Increment      => 200);
   subtype Transition_Table is Transition_Tables.Instance;

   package State_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => State_Data,
      Table_Index_Type     => State,
      Table_Low_Bound      => Start_State,
      Table_Initial        => Default_State_Count,
      Table_Increment      => 200);
   subtype State_Table is State_Tables.Instance;

   type NFA is tagged record
      States       : State_Table;
      Transitions  : Transition_Table;
      States_Are_Statefull : Boolean := True;
   end record;

   type Nested_NFA is record
      Default_Start : State;
   end record;
   No_Nested : constant Nested_NFA := (Default_Start => No_State);

   type NFA_Snapshot is record
      States      : State;
      Transitions : Transition_Id;

      Start_State_Transition : Transition_Id;
      --  Specific to the schema reader (?): since parsing other grammars will
      --  modify the start state transitions (and only this one) to add valid
      --  toplevel elements, we need to reset the list of transitions for the
      --  start state.
   end record;
   No_NFA_Snapshot : constant NFA_Snapshot :=
     (No_State, No_Transition, No_Transition);

end Sax.State_Machines;
