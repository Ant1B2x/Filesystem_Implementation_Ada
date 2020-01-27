with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with P_Tree;
with P_Array;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_File; use P_File;

package P_Folder is
   
   Same_Name_Error : Exception;
   Full_Folder_Error : Exception;

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
   
   -- Role : Overload of create, with rigth set at basic (RWX, RX, RX).
   -- Create and return a folder. Set metadata with in parameters.
   -- Parameters :
   --    name (String) : Name of the folder
   --    parent (T_Folder) : Parent of the folder beeing created
   -- Return :
   --    T_Folder : The new folder with metadata set
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; parent : in out T_Folder) return T_Folder;
   
   -- Role : Create and return a folder. Set metadata with in parameters.
   -- Parameters :
   --    name (String) : Name of the folder
   --    parent (T_Folder) : Parent of the folder beeing created
   --    rights (T_Rigths) : Special rigths given to the folder
   -- Return :
   --    T_Folder : The new folder with metadata set
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights) return T_Folder;
   
   -- Role : Return the name of given folder, as String.
   -- Parameters :
   --    folder (T_Folder) : The folder to get the name from
   -- Return :
   --    String : The name of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_name (folder : in T_Folder) return String;
   
   -- Role : Set given folder's name
   -- Parameters :
   --    folder (T_Folder) : Folder to set the new name
   --    name (String) : The new name
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_name (folder : in out T_Folder; name : in String);
   
   -- Role : Return the rights assicoated to the folder
   -- Parameters :
   --    folder (T_Folder) : Folder to get the rights from
   -- Return :
   --    T_Rights : The rigths of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_rights (folder : in T_Folder) return T_Rights;
   
   -- Role : Set the rights to the given folder
   -- Parameters :
   --    folder : (T_Folder) : Folder to set the new rights
   --    rights (T_Rights) : The new rights.
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights);
   
   -- Role : Get the size of the folder, and only the folder
   -- Parameters :
   --    folder (T_Folder) : The folder to get the size from
   -- Return :
   --    Integer : The size as bytes
   -- Preconditions : /
   -- Postconditions : the result is equal to the generic size of a folder
   function get_size (folder : in T_Folder) return Integer
   with Post => get_size'Result = FOLDER_SIZE;
   
   -- Role : Method to get the Singloton "root".
   -- Root can only be created once, and get_root will always return the same instance of root, or create it if it does not exist.
   -- As root is unic, we chosed to implement the Singloton patern.
   -- Parameters :
   --    /
   -- Return :
   --    T_Folder : The instance of root
   -- Preconditions : /
   -- Postconditions : /
   function get_root return T_Folder;
   
   -- Role : Get the path from a folder.
   -- Parameters :
   --    folder (T_Folder) : Folder to get the path from
   -- Return :
   --    String : The path of the folder.
   -- Preconditions : /
   -- Postconditions : /
   function get_path (folder : in T_Folder) return String;
   
   -- Role : Returns the path + name of a folder as a string
   -- Parameters :
   --    current_directory (in T_Folder) : Folder to get pwd from
   -- Return :
   --    return (String) : The path + name of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_pwd (current_directory : in T_Folder) return String;
   
   -- Role : Return the parent of the given folder
   -- Parameters :
   --    folder (T_Folder) : Folder to get the parent from
   -- Return :
   --    T_Folder : The parent of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_parent (folder : in T_Folder) return T_Folder;
   
   -- Role : Return if the given folder has not sub-folders or files.
   -- Parameters :
   --    folder (T_Folder) : The folder to check if empty
   -- Return :
   --    Boolean : True if the number of files and the number of folders = 0
   -- Preconditions : /
   -- Postconditions : /
   function is_empty (folder : in T_Folder) return Boolean;
   
   -- Role : Return if the given folder is null. For exemple, the parent of root.
   -- Parameters :
   --    folder (T_Folder) : The folder to check if null
   -- Return :
   --    Boolean : True if null, False if not
   -- Preconditions : /
   -- Postconditions : /
   function is_null (folder : in T_Folder) return Boolean;
   
   -- Role : Return if the giver folder is root
   -- Parameters :
   --    folder (T_Folder) : Folder to check if root
   -- Return :
   --    Boolean : Return True if folder is root, or False if not
   -- Preconditions : /
   -- Postconditions : /
   function is_root (folder : in T_Folder) return Boolean;
   
   -- Role : Return the data of the givern folder. Data countain all the files and metadata
   -- Parameters :
   --    folder (T_Folder) : Folder to get the data from
   -- Return :
   --    T_Folder_Data : Record of files and metadata
   -- Preconditions : /
   -- Postconditions : /
   function get_data (folder : in T_Folder) return T_Folder_Data;
   
   -- Role : Set the datas to the folder
   -- Parameters :
   --    folder (T_Folder) : Folder to set datas
   --    folder_data (T_Folder_Data) : Data to be set to the folder
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data);
   
   -- Role : Return the direct sub-folder n°index of folder. Sub-foders are sorted by insertion.
   -- Parameters :
   --    folder (T_Folder) : Folder to get the sub-folder from
   --    index (Integer) : Index of the wanted sub-folder
   -- Return :
   --    T_Folder : The sub-folder at index i
   -- Preconditions : /
   -- Postconditions : /
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder;
   
   -- Role : Return the number of direct sub-folders in folder
   -- Parameters :
   --    folder (T_Folder) : Folder to get the number of direct sub-folders from
   -- Return :
   --    Integer : The number of direct sub-folders
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_folders (folder : in T_Folder) return Integer;
   
   -- Role : Look for a direct sub-folder with the given folder_name. Return it if found, and null if not.
   -- Parameters :
   --    folder (T_Folder) : Folder to get sub-folder with the given name.
   --    folder_name (String) : The name of the wanted folder
   -- Return :
   --    T_Folder : The folder with folder_name as name, or null if not found
   -- Preconditions : folder_name'length <= LMAX_STRING
   -- Postconditions : /
   function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder
     with Pre => folder_name'length <= LMAX_STRING;
   
   -- Role : Delete the folder with folder_name as name from the direct sub-folders of folder.
   -- Parameters :
   --    fodler (T_Folder) : Folder to del the sub-folder from
   --    folder_name (String) : Name of the folder to delete from direct sub-folders.
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_folder (folder : in out T_Folder; folder_name : in String);
   
   -- Role : Return the direct file n°index of folder. File are sorted by insertion.
   -- Parameters :
   --    fodler (T_Folder) : Folder to get the direct dile from
   --    index (Integer) : The index of the wanted file
   -- Return :
   --    T_File : The wanted file, at index.
   -- Preconditions : /
   -- Postconditions : /
   function get_file (folder : in T_Folder; index : in Integer) return T_File
     with Pre => index <= get_nb_files(folder);
   
   -- Role : Return the number of direct files in folder
   -- Parameters :
   --    folder (T_Folder) : Folder to get the number of direct files from
   -- Return :
   --    Integer : The number of direct files
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_files (folder : in T_Folder) return Integer;
   
   -- Role : Look for a direct file with the given file_name. Return it if found, and null if not.
   -- Parameters :
   --    folder (T_Folder) : Folder to get File with the given name.
   --    folder_name (String) : The name of the wanted File
   -- Return :
   --    T_Folder : The File with file_name as name, or null if not found
   -- Preconditions : file_name'length <= LMAX_STRING
   -- Postconditions : /
   function find_file (folder : in T_Folder; file_name : in String) return T_File
     with Pre => file_name'length <= LMAX_STRING;
   
   -- Role : Add a file to the folder. At it at the end of the other files list
   -- Parameters :
   --    folder (T_Folder) : Folder to add the file in
   --    file (T_File) : File to add in folder
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure add_file (folder : in out T_Folder; file : in T_File);
   
   -- Role : Delete the file with file_name as name from the direct files of folder.
   -- Parameters :
   --    fodler (T_Folder) : Folder to del the file from
   --    file_name (String) : Name of the file to delete from direct files
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_file (folder : in out T_Folder; file_name : in String);
   
   -- Role : Check if one od the direct sub-folder or a direct file already wear this name.
   -- Parameters :
   --    folder (T_Folder) : Folder to check the existance from
   --    name (String) : Name that has to be checked
   -- Return :
   --    Boolean : Return True if a file or sub-folder has the same name that the on given in parameter, or False if not
   -- Preconditions : /
   -- Postconditions : /
   function has_son_with_this_name(folder: T_Folder; name: String) return Boolean;
   
   -- Role : Check if current_directory has as parent supposed_parent
   -- Parameters :
   --    current_directory (T_Folder) : Folder to check the supposed parent from
   --    supposed_parent (T_Folder) : The supposed parent
   -- Return :
   --    Boolean : True if current_directory has supposed_parent as parent, False if not
   -- Preconditions : /
   -- Postconditions : /
   function has_as_parent(current_directory : T_Folder; supposed_parent : T_Folder) return Boolean;
   
private
   ROOT : T_Folder;
   -- Role : Create a special folder without parent, and with specific caracteristics, specifics to root
   -- Parameters :
   --    /
   -- Return :
   --    T_Folder : The root folder
   -- Preconditions : /
   -- Postconditions : /
   function create_root return T_Folder;
   
   -- Role : Calculate the actual absolute path of a folder.
   -- Parameters :
   --    folder (T_Folder) : Folder to calculate the path
   -- Return :
   --    String : A String representing the path from root
   -- Preconditions : /
   -- Postconditions : /
   function calculate_path (folder : in T_Folder) return String;
   
   -- Role : Calculate the actual absolute path of a folder.
   -- Parameters :
   --    folder1 (T_Folder) : First folder to compare
   --     :  (T_Folder) : Second folder to compare
   -- Return :
   --    Boolean : True if they are the same folder, False if not
   -- Preconditions : /
   -- Postconditions : /
   function are_the_same(folder1 : T_Folder; folder2 : T_Folder) return Boolean;
   
end P_Folder;
