with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core;              use DOM.Core;

package Driver is
   procedure Assert_Equals
     (Expected    : DOM_String;
      Actual      : DOM_String;
      Ignore_Case : Boolean;
      File        : String;
      Id          : String);
   procedure Assert_Equals
     (Expected    : DOM_String;
      Actual      : Unbounded_String;
      Ignore_Case : Boolean;
      File        : String;
      Id          : String);
   procedure Assert_Null
     (Actual      : Node;
      File        : String;
      Id          : String);
   procedure Assert_Not_Null
     (Actual      : Node;
      File        : String;
      Id          : String);
   procedure Assert_Not_Null
     (Actual      : Named_Node_Map;
      File        : String;
      Id          : String);
   procedure Assert_False
     (Actual      : Boolean;
      File        : String;
      Id          : String);
   procedure Assert_True
     (Actual      : Boolean;
      File        : String;
      Id          : String);

end Driver;
