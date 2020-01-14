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
      nb_files : Integer; -- effective files number
      files : T_Files;
   end record;
   
   -- instanciation of Tree Package
   -- P_Folder_Tree.T_Tree will be our Folder type
   package P_Folder_Tree is new P_Tree (T => T_Folder_Data);
   subtype T_Folder is P_Folder_Tree.T_Tree;
   subtype T_Folders is P_Folder_Tree.T_Siblings;
   -- use type T_Folder; ?
   
   function create (name : in String; parent : in T_Folder; path : in String) return T_Folder;
   
   function create (name : in String; parent : in T_Folder; rights : in T_Rights; path : in String) return T_Folder;
   
   function get_name (folder : in T_Folder) return String;
   
   procedure set_name (folder : in out T_Folder; name : in String);
   
   function get_rights (folder : in T_Folder) return T_Rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights);
   
   function get_size (folder : in T_Folder) return Integer;
   
   function get_path (folder : in T_Folder) return String;
   
   procedure set_path (folder : in out T_Folder; path : in String);
   
   function get_parent (folder : in T_Folder) return T_Folder;
   
   procedure set_parent (folder : in out T_Folder; parent : in T_Folder);
   
   function is_empty (folder : in T_Folder) return Boolean;
   
   function get_folders (folder : in T_Folder) return T_Folders;
   
   function get_nb_folders (folder : in T_Folder) return Integer;
   
   procedure add_folder (folder : in out T_Folder; sibling_folder : in T_Folder);
   
   procedure del_folder (folder : in out T_Folder; sibling_folder : in T_Folder);
   
   function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder
     with Pre => folder_name'length <= LMAX_STRING;
   
   function get_files (folder : in T_Folder) return T_Files;
   
   procedure add_file (folder : in out T_Folder; file : in T_File);
   
   procedure del_file (folder : in out T_Folder; file : in T_File);
   
   function find_file (folder : in T_Folder; file_name : in String) return T_File
     with Pre => file_name'length <= LMAX_STRING;
   
end P_Folder;
