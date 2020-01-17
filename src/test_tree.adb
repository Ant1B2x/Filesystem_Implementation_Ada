with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Tree;

procedure test_tree is
   
   package P_Tree_Integer is new P_Tree (T => Integer);
   use P_Tree_Integer;
   
   tree_integer : P_Tree_Integer.T_Tree;
   tree_integer_sibling : P_Tree_Integer.T_Tree;
   tree_integer_parent : P_Tree_Integer.T_Tree;
begin
   -- create
   put_line("Create:");
   tree_integer := create;
   if is_empty(tree_integer) then
      put_line("is_empty(tree_integer) = True");
   else
      put_line("is_empty(tree_integer) is incoherent");
   end if;
   new_line;
   
   -- set data & get data
   put_line("Set data & get data:");
   set_data(tree_integer, 50);
   if get_data(tree_integer) = 50 then
      put_line("get_data(tree) = 50");
   else
      put_line("get_data(tree) is incoherent");
   end if;
   new_line;
       
   -- add sibling & get nb siblings
   put_line("Add sibling & get_nb_siblings:");
   tree_integer_sibling := create;
   set_data(tree_integer_sibling, 100);
   add_sibling(tree_integer, tree_integer_sibling);
   if get_nb_siblings(tree_integer) = 1 then
      put_line("get_nb_siblings(tree_integer) = 1, we just added a new sibling");
   else
      put_line("add_sibling(tree_integer, tree_integer_sibling) didn't work, tree empty");
   end if;
   new_line;
   
   -- is empty
   put_line("Is empty:");
   if not is_empty(tree_integer) then
      put_line("is_empty(tree_integer) = False, we have 1 sibling");
   else
      put_line("is_empty(tree_integer) is incoherent");
   end if;
   new_line;
   
   -- get sibling
   put_line("Get sibling:");
   if get_sibling(tree_integer, 1) = tree_integer_sibling then
      put_line("get_sibling(tree_integer, 1) = tree_integer_sibling, returned correct sibling");
   else
      put_line("get_sibling(tree_integer, 1) is incoherent");
   end if;
   new_line;
   
   -- del sibling
   put_line("Del sibling:");
   del_sibling(tree_integer, tree_integer_sibling);
   if is_empty(tree_integer) then
      put_line("is_empty(tree_integer) = True, we deleted the sibling");
   else
      put_line("add_sibling(tree_integer, tree_integer_sibling) didn't work, tree not empty");
   end if;
   new_line;
   
   -- is null
   put_line("Is null:");
   if is_null(tree_integer_parent) then
      put_line("is_null(tree_integer_parent) = True, we never initialized it");
   else
      put_line("is_null(tree_integer_parent) is incoherent");
   end if;
   new_line;
   
   -- set parent & get parent
   put_line("Set parent & get parent:");
   tree_integer_parent := create;
   set_data(tree_integer_parent, 25);
   set_parent(tree_integer, tree_integer_parent);
   if get_parent(tree_integer) = tree_integer_parent then
      put_line("get_parent(tree_integer) = tree_integer_parent");
   else
      put_line("get_parent(tree_integer) is incoherent");
   end if;
   new_line;
   
   -- create with parent
   put_line("Create with parent:");
   tree_integer := create(tree_integer_parent); -- reset tree_integer
   set_data(tree_integer, 50);
   if get_parent(tree_integer) = tree_integer_parent then
      put_line("get_parent(tree_integer) = tree_integer_parent");
   else
      put_line("get_parent(tree_integer) is incoherent");
   end if;
   new_line;
   
end test_tree;
