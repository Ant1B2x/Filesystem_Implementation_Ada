with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_Tree;
with P_File; use P_File;

package P_Folder is

   -- files of the folder
   type T_Files is array (1..NMAX_SIBLINGS) of T_File;
   
   -- data contained in a folder (metadata + files)
   -- folder siblings are already present
   -- indeed, they're the siblings in the tree
   type T_Folder_Data is record
      metadata : T_Metadata;
      nb_files; -- effective files number
      files : T_Files;
   end record;
   
   -- instanciation of Tree Package
   -- P_Folder_Tree.T_Tree will be our Folder type
   package P_Folder_Tree is new P_Tree (T => T_Folder_Data);
   subtype T_Folder is P_Folder_Tree.T_Tree;
   subtype T_Folder_Siblings is P_Folder_Tree.T_Siblings;
   
   function create (name : in String) return T_Folder;
   
   function create (name : in String; rights : in T_Rights) return T_Folder;
   
   function get_name (folder : in T_Folder) return String;
   
   procedure set_name (folder : in out T_Folder; name : in String)
     with Pre => name'length <= LMAX_NAME;
   
   function get_rights (folder : in T_Folder) return T_Rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights);
   
   function get_size (folder : in T_Folder) return Integer;
   
   procedure set_size (folder : in out T_Folder; size : in Integer)
     with Pre => size <= SMAX_FILE;
   
   function get_parent (folder : in T_Folder) return T_Folder;
   
   procedure set_parent (folder : in out T_Folder; parent : in T_Folder);
   
   function is_empty (folder : in T_Folder) return Boolean;
   
   function get_siblings (folder : in T_Folder) return T_Folder_Siblings;
   
   procedure add_sibling (folder : in out T_Folder; sibling : in T_Folder)
     with Pre => folder.all.nb_siblings <= NMAX_SIBLINGS;
   
   procedure del_sibling (folder : in out T_Folder; sibling : in P_Folder_Tree.T_Tree);
   
   function get_files (folder : in T_Folder) return T_Files;
   
   procedure add_file (folder : in out T_Folder; file : in T_File);
   
   procedure del_file (folder : in out T_Folder; file : in T_File);
   
   
   
   -- ajouter / supprimer fils
   
   
   

end P_Folder;
