with P_Constants; use P_Constants;
with P_Array;

generic
   type T is private;

package P_Tree is
   
   type T_Node is private;
   type T_Tree is access T_Node; -- T_Tree should be in "protected" because we need to know that it's a pointer
   
   package P_Siblings is new P_Array (T => T_Tree);
   subtype T_Siblings is P_Siblings.T_Array;
   
   -- Role : Create an empty tree
   -- Parameters :
   --    / 
   -- Return :
   --    T_Tree : The new tree
   -- Preconditions : /
   -- Postconditions : /
   function create return T_Tree;
   
   -- Role : Create a Tree and specify it's parent. Add it as sibling of its parent
   -- Parameters :
   --    parent (in out T_Tree) : The parent of the current tree
   -- Return :
   --    T_Tree : The new tree
   -- Preconditions : /
   -- Postconditions : /
   function create (parent : in out T_Tree) return T_Tree;
   
   -- Role : Return if tree has siblings
   -- Parameters :
   --    tree (in String) : The tree to check if empty or not
   -- Return :
   --    Boolean : Return True if tree has no siblings, False if not
   -- Preconditions : /
   -- Postconditions : /
   function is_empty (tree : in T_Tree) return Boolean;
   
   -- Role : Return if tree is null
   -- Parameters :
   --    tree (in T_Tree) : The tree to check if null or not
   -- Return :
   --    Boolean : Return True if tree is null, False if not
   -- Preconditions : /
   -- Postconditions : /
   function is_null (tree : in T_Tree) return Boolean;
   
   -- Role : Return the data of tree, as T (generic type)
   -- Parameters :
   --    tree (in T_Tree) : The tree to get the data from
   -- Return :
   --    T : Data as T (generic type)
   -- Preconditions : /
   -- Postconditions : /
   function get_data (tree : in T_Tree) return T;
   
   -- Role : Set data to a tree
   -- Parameters :
   --    tree (in out T_Tree) : Tree to set data
   --    data (in T) : Data as T (generic type)
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_data (tree : in out T_Tree; data : in T);
   
   -- Role : Return the parent of tree
   -- Parameters :
   --    tree (in T_Tree) : Tree to get the parent from
   -- Return :
   --    T_Tree : The parent of tree
   -- Preconditions : /
   -- Postconditions : /
   function get_parent (tree : in T_Tree) return T_Tree;
   
   -- Role : Return the sibling at index n°index. Siblings are sorted by insertion.
   -- Parameters :
   --    tree (in T_Tree) : Tree to get the sibling from
   --    index (in Integer) : index of the wanted sibling
   -- Return :
   --    T_Tree : The wanted sibling
   -- Preconditions : /
   -- Postconditions : /
   function get_sibling (tree : in T_Tree; index : in Integer) return T_Tree;
   
   -- Role : Return the number of tree siblings. Siblings are sorted by insertion.
   -- Parameters :
   --    tree (in T_Tree) : Tree to get the siblings from
   -- Return :
   --    Integer : The number of current siblings
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_siblings (tree : in T_Tree) return Integer;
   
   -- Role : Add a sibling to tree. Siblings are sorted by insertion.
   -- Parameters :
   --    tree (in out T_Tree) : The tree to add sibling
   --    sibling (in out T_Tree) : The sibling to add
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure add_sibling (tree : in out T_Tree; sibling : in out T_Tree);
   
   -- Role : Delete the wanted sibling from tree
   -- Parameters :
   --    tree (in out T_Tree) : The tree to delete from
   --    sibling (in out T_Tree) : the sibling to delete
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_sibling (tree : in out T_Tree; sibling : in out T_Tree);
   
private
   
   type T_Node is record
      data : T;
      parent : T_Tree;
      siblings : T_Siblings;
   end record;
   
   -- called by add_sibling and del_sibling
   -- Role : Set a new parent to tree
   -- Parameters :
   --    tree (in out T_Tree) : The tree to set parent
   --    parent (in T_Tree) : The new parent of tree
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_parent (tree : in out T_Tree; parent : in T_Tree);

end P_Tree;
