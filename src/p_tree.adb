package body P_Tree is

   function create return T_Tree is
      tree : T_Tree;
   begin
      tree := new T_Node;
      tree.all.parent := null;
      tree.all.nb_siblings := 0;
      return tree;
   end create;
   
   function create (parent : in T_Tree) return T_Tree is
      tree : T_Tree;
   begin
      tree := new T_Node;
      tree.all.parent := parent;
      tree.all.nb_siblings := 0;
      return tree;
   end create;
   
   function is_empty (tree : in T_Tree) return Boolean is
   begin
      return tree.all.nb_siblings = 0;
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
   
   -- return an array with the effective number of siblings
   function get_siblings (tree : in T_Tree) return T_Siblings is
   begin
      return tree.all.siblings(1..tree.all.nb_siblings);
   end get_siblings;
   
   procedure add_sibling (tree : in out T_Tree; sibling : in T_Tree) is
   begin
      tree.all.nb_siblings := tree.all.nb_siblings + 1;
      tree.all.siblings(tree.all.nb_siblings) := sibling;
   end add_sibling;      
   
   procedure del_sibling (tree : in out T_Tree; sibling : in T_Tree) is
      index : Integer;
   begin
      -- look for the sibling
      index := 1;
      while index <= tree.all.nb_siblings
        and then tree.all.siblings(index) /= sibling loop
      index := index + 1;   
      end loop;
      -- if the sibling is found
      if index <= tree.all.nb_siblings then
         -- compact
         while index < tree.all.nb_siblings loop
            tree.all.siblings(index) := tree.all.siblings(index + 1);
            index := index + 1;
         end loop;
         -- delete an element
         tree.all.nb_siblings := tree.all.nb_siblings - 1;
      end if;
   end del_sibling;

end P_Tree;
