with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with P_Tree;
with P_Array;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_File; use P_File;

package P_Folder is
   
   -- raised when trying to create new file or folder and a file or folder with the same name already exists in the directory
   Same_Name_Error : Exception;
   -- raised when trying to add new file or folder and the directory is already full
   Full_Folder_Error : Exception;

   -- files of the folder, array of files
   package P_Files is new P_Array (T => T_File);
   subtype T_Files is P_Files.T_Array;
   
   -- data contained in a folder (metadata + files)
   -- folder siblings are already present
   -- indeed, they're the siblings in the tree
   type T_Folder_Data is record
      metadata : T_Metadata; -- metadata of the folder
      files : T_Files; -- file array of the folder
   end record;
   
   -- instanciation of Tree Package
   -- P_Folder_Tree.T_Tree will be our Folder type
   package P_Folder_Tree is new P_Tree (T => T_Folder_Data);
   subtype T_Folder is P_Folder_Tree.T_Tree;
   use type T_Folder;
   
   -- Role : Create and return a folder, set its metadata to the given parameters
   -- Parameters :
   --    name (in String) : Name of the folder
   --    parent (in out T_Folder) : Parent of the folder beeing created, need to be in "in / out" because we're going to modify its parent
   --    rights (in T_Rigths) : Special rigths given to the folder
   -- Return :
   --    T_Folder : The new folder with metadata set
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights) return T_Folder;
   
   -- Role : Overload of create, with rigths set at 755
   -- Create and return a folder, set its metadata to the given parameters
   -- Parameters :
   --    name (in String) : Name of the folder
   --    parent (in out T_Folder) : Parent of the folder beeing created, need to be in "in / out" because we're going to modify its parent
   -- Return :
   --    T_Folder : The new folder with metadata set
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; parent : in out T_Folder) return T_Folder;
   
   -- Role : Return the name of given folder, as String
   -- Parameters :
   --    folder (in T_Folder) : The folder to get the name from
   -- Return :
   --    String : The name of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_name (folder : in T_Folder) return String;
   
   -- Role : Set given name to a folder
   -- Parameters :
   --    folder (in out T_Folder) : Folder to set the new name
   --    name (in String) : The new name
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_name (folder : in out T_Folder; name : in String);
   
   -- Role : Return the rights associated to a folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the rights from
   -- Return :
   --    T_Rights : The rights of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_rights (folder : in T_Folder) return T_Rights;
   
   -- Role : Set given rights to a folder
   -- Parameters :
   --    folder : (in out T_Folder) : Folder to set the new rights
   --    rights (in T_Rights) : The new rights
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights);
   
   -- Role : Get the size of the folder, and only the folder
   -- Parameters :
   --    folder (in T_Folder) : The folder to get the size from
   -- Return :
   --    Integer : The size as bytes
   -- Preconditions : /
   -- Postconditions : the result is equal to the generic size of a folder
   function get_size (folder : in T_Folder) return Integer
     with Post => get_size'Result = FOLDER_SIZE;
   
   -- Role : Method to get the singleton "root"
   -- Root can only be created once, and get_root will always return the same instance of root, or create it if it does not exist
   -- As root is unique, we chose to implement the singleton pattern
   -- Parameters : /
   -- Return :
   --    T_Folder : The instance of root
   -- Preconditions : /
   -- Postconditions : /
   function get_root return T_Folder;
   
   -- Role : Get the path from a folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the path from
   -- Return :
   --    String : The path of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_path (folder : in T_Folder) return String;
   
   -- Role : Return the pwd (path + name) of a folder as a string
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the pwd from
   -- Return :
   --    return (String) : The pwd of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_pwd (folder : in T_Folder) return String;
   
   -- Role : Return the parent of the given folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the parent from
   -- Return :
   --    T_Folder : The parent of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_parent (folder : in T_Folder) return T_Folder;
   
   -- Role : Check if a folder is empty or not
   -- Parameters :
   --    folder (in T_Folder) : The folder to check if empty
   -- Return :
   --    Boolean : True if the number of files and the number of folders = 0, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function is_empty (folder : in T_Folder) return Boolean;
   
   -- Role : Check if a folder is null or not
   -- Parameters :
   --    folder (in T_Folder) : The folder to check if null
   -- Return :
   --    Boolean : True if the folder is null, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function is_null (folder : in T_Folder) return Boolean;
   
   -- Role : Check if a folder is root or not
   -- Parameters :
   --    folder (in T_Folder) : Folder to check if root
   -- Return :
   --    Boolean : True if the folder is root, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function is_root (folder : in T_Folder) return Boolean;
   
   -- Role : Compare two folders
   -- Parameters :
   --    folder1 (in T_Folder) : First folder to compare
   --    folder2 (in T_Folder) : Second folder to compare
   -- Return :
   --    Boolean : True if they are the same folder, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function are_the_same(folder1 : in T_Folder; folder2 : in T_Folder) return Boolean;
   
   -- Role : Return the data of a given folder, data contain all the files and metadata
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the data from
   -- Return :
   --    T_Folder_Data : Record of files and metadata associated to the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_data (folder : in T_Folder) return T_Folder_Data;
   
   -- Role : Set data to a folder
   -- Parameters :
   --    folder (in out T_Folder) : Folder to set data
   --    folder_data (in T_Folder_Data) : Data to set to the folder
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data);
   
   -- Role : Return the sub-folder at a specified index, sub-foders are sorted by insertion
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the sub-folder from
   --    index (in Integer) : Index of the wanted sub-folder
   -- Return :
   --    T_Folder : The sub-folder at index i
   -- Preconditions : /
   -- Postconditions : /
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder;
   
   -- Role : Return the number of direct sub-folders in a folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the number of direct sub-folders from
   -- Return :
   --    Integer : The number of direct sub-folders of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_folders (folder : in T_Folder) return Integer;
   
   -- Role : Find a direct sub-folder with a given name, return it if found, or null otherwise
   -- Parameters :
   --    current_folder (in T_Folder) : Folder to get the sub-folder from
   --    folder_name (in String) : The name of the wanted sub-folder
   -- Return :
   --    T_Folder : The sub-folder with the given name, or null otherwise
   -- Preconditions : the folder name length is inferior or equal to the max length of a string
   -- Postconditions : /
   function find_folder (current_folder : in T_Folder; folder_name : in String) return T_Folder
     with Pre => folder_name'length <= LMAX_STRING;
   
   -- Role : Delete a sub-folder with a given name inside a given folder
   -- Parameters :
   --    folder (in out T_Folder) : Folder to del the sub-folder from
   --    folder_name (in String) : Name of the folder to delete from direct sub-folders
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_folder (folder : in out T_Folder; folder_name : in String);
   
   -- Role : Return the file at a specified index, files are sorted by insertion
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the direct file from
   --    index (in Integer) : The index of the wanted file
   -- Return :
   --    T_File : The wanted file, at index i
   -- Preconditions : 
   -- Postconditions : /
   function get_file (folder : in T_Folder; index : in Integer) return T_File;
   
   -- Role : Return the number of direct files in a folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to get the number of direct files from
   -- Return :
   --    Integer : The number of direct files in the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_files (folder : in T_Folder) return Integer;
   
   -- Role : Find a direct file with a given name, return it if found, or null otherwise
   -- Parameters :
   --    current_folder (in T_Folder) : Folder to get file with the given name
   --    folder_name (in String) : The name of the wanted File
   -- Return :
   --    T_Folder : The file with the given name, or null otherwise
   -- Preconditions : the file name length is inferior or equal to the max length of a string
   -- Postconditions : /
   function find_file (current_folder : in T_Folder; file_name : in String) return T_File
     with Pre => file_name'length <= LMAX_STRING;
   
   -- Role : Add a file to a folder (at the end of the file list)
   -- Parameters :
   --    folder (in out T_Folder) : Folder to add the file in
   --    file (in T_File) : File to add to the folder
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure add_file (folder : in out T_Folder; file : in T_File);
   
   -- Role : Delete the file with a given name from the direct files of a folder
   -- Parameters :
   --    fodler (in out T_Folder) : Folder to delete the file from
   --    file_name (in String) : Name of the file to delete from direct files
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_file (folder : in out T_Folder; file_name : in String);
   
   -- Role : Check if one of the direct sub-folder or a direct file already has a given name
   -- Parameters :
   --    folder (in T_Folder) : Folder to check the sub-folders and files names
   --    name (in String) : Name that has to be checked
   -- Return :
   --    Boolean : True if a file or sub-folder already has the given name, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function has_son_with_this_name (folder : in T_Folder; name : in String) return Boolean;
   
   -- Role : Check if a folder has a supposed parent as parent
   -- Parameters :
   --    folder (in T_Folder) : Folder to check the supposed parent from
   --    supposed_parent (in T_Folder) : The supposed parent
   -- Return :
   --    Boolean : True if the folder has supposed_parent as parent, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function has_parent (folder : in T_Folder; supposed_parent : in T_Folder) return Boolean;
   
private
   -- singleton unique root folder
   ROOT : T_Folder;
   
   -- Role : Create a special folder without parent, and with the specific caracteristics of root
   -- Should only be called once, as it is used by get_root
   -- Parameters : /
   -- Return :
   --    T_Folder : The root folder
   -- Preconditions : /
   -- Postconditions : /
   function create_root return T_Folder;
   
   -- Role : Calculate the actual absolute path of a folder
   -- Parameters :
   --    folder (in T_Folder) : Folder to calculate the path
   -- Return :
   --    String : A String representing the absolute path from root to the folder
   -- Preconditions : /
   -- Postconditions : /
   function calculate_path (folder : in T_Folder) return String;
   
end P_Folder;
