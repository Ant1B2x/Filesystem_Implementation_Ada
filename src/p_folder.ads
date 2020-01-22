with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_Tree;
with P_Array;
with P_File; use P_File;

package P_Folder is
   
   Same_Name_Error : Exception;
   Invalid_Folder_Error: Exception;

   -- files of the folder   
   package P_Files is new P_Array (T => T_File);
   subtype T_Files is P_Files.T_Array;
   
   -- data contained in a folder (metadata + files)
   -- folder siblings are already present
   -- indeed, they're the siblings in the tree
   type T_Folder_Data is record
      metadata : T_Metadata;
      files : T_Files;
   end record;
   
   -- instanciation of Tree Package
   -- P_Folder_Tree.T_Tree will be our Folder type
   package P_Folder_Tree is new P_Tree (T => T_Folder_Data);
   subtype T_Folder is P_Folder_Tree.T_Tree;
   use type T_Folder;
   
   function create (name : in String; parent : in out T_Folder) return T_Folder;
   
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights) return T_Folder;
   
   function get_name (folder : in T_Folder) return String;
   
   procedure set_name (folder : in out T_Folder; name : in String);
   
   function get_rights (folder : in T_Folder) return T_Rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights);
   
   function get_size (folder : in T_Folder) return Integer;
   
   function get_root return T_Folder;
   
   function get_path (folder : in T_Folder) return String;
   
   function get_parent (folder : in T_Folder) return T_Folder;
   
   function is_empty (folder : in T_Folder) return Boolean;
   
   function is_null (folder : in T_Folder) return Boolean;
   
   function is_root (folder : in T_Folder) return Boolean;
   
   function get_data (folder : in T_Folder) return T_Folder_Data;
   
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data);
   
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder;
   
   function get_nb_folders (folder : in T_Folder) return Integer;
   
   function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder
     with Pre => folder_name'length <= LMAX_STRING;
   
   procedure del_folder (folder : in out T_Folder; folder_name : in String);
   
   function get_file (folder : in T_Folder; index : in Integer) return T_File
     with Pre => index <= get_nb_files(folder);
   
   function get_nb_files (folder : in T_Folder) return Integer;
   
   function find_file (folder : in T_Folder; file_name : in String) return T_File
     with Pre => file_name'length <= LMAX_STRING;
   
   procedure add_file (folder : in out T_Folder; new_file : in T_File);
   
   procedure del_file (folder : in out T_Folder; file_name : in String);
   
   function has_son_with_this_name(folder: T_Folder; name: String) return Boolean;
   
private
   ROOT : T_Folder;
   function create_root return T_Folder;
   function calculate_path (folder : in T_Folder) return String;
   
end P_Folder;
