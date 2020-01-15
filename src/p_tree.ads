with P_Constants; use P_Constants;
with P_Array;

generic
   type T is private;

package P_Tree is
   
   type T_Node is private;
   type T_Tree is access T_Node;
   
   package P_Siblings is new P_Array (T => T_Tree);
   subtype T_Siblings is P_Siblings.T_Array;
   
   function create return T_Tree;
   
   function create (parent : in T_Tree) return T_Tree;
   
   function is_empty (tree : in T_Tree) return Boolean;
   
   function get_data (tree : in T_Tree) return T;
   
   procedure set_data (tree : in out T_Tree; data : in T);
   
   function get_parent (tree : in T_Tree) return T_Tree;
   
   procedure set_parent (tree : in out T_Tree; parent : in T_Tree);
   
   function get_sibling (tree : in T_Tree; index : in Integer) return T_Tree
     with Pre => index <= get_nb_siblings(tree);
   
   function get_nb_siblings (tree : in T_Tree) return Integer;
   
   procedure add_sibling (tree : in out T_Tree; sibling : in T_Tree);
   
   procedure del_sibling (tree : in out T_Tree; sibling : in T_Tree);
      
private
   
   type T_Node is record
      data : T;
      parent : T_Tree;
      siblings : T_Siblings;
   end record;

end P_Tree;
