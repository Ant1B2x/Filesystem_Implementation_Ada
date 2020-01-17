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
   put_line("get_nb_siblings");
   tree_integer_sibling := create;
   set_data(tree_integer_sibling, 100);
   add_sibling(tree_integer, tree_integer_sibling);
   
   
   -- is empty
   put_line("Is empty:");
   if not is_empty(tree_integer) then
      put_line("is_empty(tree_integer) = True");
   else
      put_line("is_empty(tree_integer) is incoherent");
   end if;
   new_line;
   
   -- create with parent
   put_line("Create with parent:");
   
   
   
   --function create (parent : in T_Tree) return T_Tree;
   
   --function is_empty (tree : in T_Tree) return Boolean;
   
   --function get_parent (tree : in T_Tree) return T_Tree;
   
   --procedure set_parent (tree : in out T_Tree; parent : in T_Tree);
   
   --function get_sibling (tree : in T_Tree; index : in Integer) return T_Tree
     --with Pre => index <= get_nb_siblings(tree);
   
   --function get_nb_siblings (tree : in T_Tree) return Integer;
   
   --procedure add_sibling (tree : in out T_Tree; sibling : in T_Tree);
   
   --procedure del_sibling (tree : in out T_Tree; sibling : in T_Tree);
   
   
   
   
end test_tree;
