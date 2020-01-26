with P_Constants; use P_Constants;
with P_Array;

generic
   type T is private;

package P_Tree is
   
   type T_Node is private;
   type T_Tree is access T_Node; -- T_Tree should be in "protected" because we need to know that it's a pointer
   
   package P_Siblings is new P_Array (T => T_Tree);
   subtype T_Siblings is P_Siblings.T_Array;
   
   function create return T_Tree;
   
   function create (parent : in out T_Tree) return T_Tree;
   
   function is_empty (tree : in T_Tree) return Boolean;
   
   function is_null (tree : in T_Tree) return Boolean;
   
   function get_data (tree : in T_Tree) return T;
   
   procedure set_data (tree : in out T_Tree; data : in T);
   
   function get_parent (tree : in T_Tree) return T_Tree;
   
   function get_sibling (tree : in T_Tree; index : in Integer) return T_Tree;
   
   function get_nb_siblings (tree : in T_Tree) return Integer;
   
   procedure add_sibling (tree : in out T_Tree; sibling : in out T_Tree);
   
   procedure del_sibling (tree : in out T_Tree; sibling : in out T_Tree);
   
private
   
   type T_Node is record
      data : T;
      parent : T_Tree;
      siblings : T_Siblings;
   end record;
   
   -- called by add_sibling and del_sibling
   procedure set_parent (tree : in out T_Tree; parent : in T_Tree);

end P_Tree;
