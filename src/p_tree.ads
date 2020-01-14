with P_Constants; use P_Constants;

generic
   type T is private;

package P_Tree is
   
   type T_Node is private;
   type T_Tree is access T_Node;
   
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
   
   procedure add_sibling (tree : in out T_Tree; sibling : in T_Tree)
     with Pre => get_nb_siblings(tree) <= NMAX_SIBLINGS;
   
   procedure del_sibling (tree : in out T_Tree; sibling : in T_Tree);
      
private
   
   type T_Siblings is array (1..NMAX_SIBLINGS) of T_Tree;
   
   type T_Node is record
      data : T;
      parent : T_Tree;
      nb_siblings : Integer; -- number of effective siblings in the node
      siblings : T_Siblings;
   end record;

end P_Tree;
