package body P_Tree is

   function create return T_Tree is
      tree : T_Tree;
   begin
      tree := new T_Node;
      tree.all.parent := null;
      tree.all.siblings := P_Siblings.create;
      return tree;
   end create;
   
   function create (parent : in T_Tree) return T_Tree is
      tree : T_Tree;
   begin
      tree := create;
      set_parent(tree, parent);
      return tree;
   end create;
   
   function is_empty (tree : in T_Tree) return Boolean is
   begin
      return get_nb_siblings(tree) = 0;
   end is_empty;
   
   function get_data (tree : in T_Tree) return T is
   begin
      return tree.all.data;
   end get_data;
   
   procedure set_data (tree : in out T_Tree; data : in T) is
   begin
      tree.all.data := data;
   end set_data;
   
   function get_parent (tree : in T_Tree) return T_Tree is
   begin
      return tree.all.parent;
   end get_parent;
   
   procedure set_parent (tree : in out T_Tree; parent : in T_Tree) is
   begin
      tree.all.parent := parent;
   end set_parent;
   
   function get_sibling (tree : in T_Tree; index : in Integer) return T_Tree is
   begin
      return P_Siblings.get_value(tree.all.siblings, index);
   end get_sibling;
   
   function get_nb_siblings (tree : in T_Tree) return Integer is
   begin
      return P_Siblings.get_nb_values(tree.all.siblings);
   end get_nb_siblings;
   
   procedure add_sibling (tree : in out T_Tree; sibling : in T_Tree) is
   begin
      P_Siblings.add_value(tree.all.siblings, sibling);
   end add_sibling;
   
   procedure del_sibling (tree : in out T_Tree; sibling : in T_Tree) is
   begin
      P_Siblings.del_value(tree.all.siblings, sibling);
   end del_sibling;

end P_Tree;
