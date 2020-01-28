package body P_Tree is

   function create return T_Tree is
      tree : T_Tree;
   begin
      tree := new T_Node;
      tree.all.parent := null;
      tree.all.siblings := P_Siblings.create;
      return tree;
   end create;
   
   function create (parent : in out T_Tree) return T_Tree is
      tree : T_Tree;
   begin
      tree := create;
      add_sibling(parent, tree);
      return tree;
   end create;
   
   function is_empty (tree : in T_Tree) return Boolean is
   begin
      return get_nb_siblings(tree) = 0;
   end is_empty;
   
   function is_null (tree : in T_Tree) return Boolean is
   begin
      return tree = null;
   end is_null;
   
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
   
   procedure add_sibling (tree : in out T_Tree; sibling : in out T_Tree) is
      sibling_parent : T_Tree;
   begin
      -- if the parent of the sibling is not null
      if not is_null(get_parent(sibling)) then
         -- delete the sibling from his parent
         sibling_parent := get_parent(sibling);
         del_sibling(sibling_parent, sibling);
      end if;
      -- add the sibling to the list of siblings of tree
      P_Siblings.add_value(tree.all.siblings, sibling);
      -- set tree as the parent of sibling
      set_parent(sibling, tree);
   end add_sibling;
   
   procedure del_sibling (tree : in out T_Tree; sibling : in out T_Tree) is
   begin
      -- R0 : Supprimer un direct direct
      -- R1 : Supprimer le lien vers le fils dans le père(R1.1)
      --      Enlever le lien vers le père dans le fils(R1.2)
      
      -- R2.1 : Comment (R1.1)
      --     -----> Voir P_Siblings.del_value
      -- R2.2 : Comment (R1.2)
      --     definir_parent(fils, null)
      
      
      -- delete the sibling from the list of siblings of tree
      P_Siblings.del_value(tree.all.siblings, sibling);
      -- reset the parent of sibling
      set_parent(sibling, null);
   end del_sibling;

end P_Tree;
