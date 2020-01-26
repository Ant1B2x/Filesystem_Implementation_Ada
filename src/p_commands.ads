with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Ordered_Sets;

with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_File; use P_File;
with P_Substrings; use P_Substrings;

package P_Commands is
   
   Wrong_Arguments_Number_Error : Exception;
   Option_Not_Handled_Error : Exception;
   Invalid_Option_Error : Exception;
   Invalid_Folder_Error : Exception;

   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch, help, clear);
   
   function command_to_string (encoded_command : in encoded_commands) return String;
   
   -- Role : Its a function returning the correct path. As it is use in command and in Terminal, it has to be a function
   -- Parameters :
   --    current_directory (in T_Folder) : Folder to get path from
   -- Return :
   --    return (String) : The path of the folder
   -- Preconditions : /
   -- Postconditions : /
   function get_pwd (current_directory : in T_Folder) return String;
   
   procedure run_command(current_directory : in out T_Folder; command: in String);
   
private
   
   
   -- Role : Print the path of the current folder
   -- Parameters :
   --    current_directory (T_Folder) : The current folder to display the path from
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure pwd_command (current_directory: in T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Print the current folders and files inside of the current folder. Sort alphabetically. Folders are print with color.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user. Only "-r" is accepted, and do a recursive ls.
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure ls_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Delete a file or a folder, with absolute or relative path, passed as parameter.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user.Use "-r" to delete a folder. 
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure rm_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Change the current directory, to move in the tree of folders. Can only we used for existing folders, by absolute or relative path as parameter.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user. No options accepted.
   --    parameters (T_Substrings) : parameters entered by the user. The path, relatve or absolute.
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure cd_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Create the wanted directory, with the absolute or relative path passed as parameter.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    
   -- Preconditions : /
   -- Postconditions : /
   procedure mkdir_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Copy a file , or a directory with "-r" option. 
   -- Use 2 parameters (path from copy and path to copy), and you have to specify the name of the new copy. 
   -- Copy all the content of the first parameter into a new folder, with the new name you gave, in the path you gave.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    
   -- Preconditions : /
   -- Postconditions : /
   procedure cp_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Move the a file , or a directory with "-r" option. 
   -- Use 2 parameters (path from copy and path to copy), and you have to specify the name of the new copy. 
   -- Move all the content of the first parameter into a new folder, with the new name you gave, in the path you gave.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure mv_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Create a .tar file with the name of the folder you want to compress.
   -- The weigth of this archive is the sum of all folder, file, sub-folders and sub-files of the specified folder.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure tar_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Create a new file. Need a path with name as last entity.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure touch_command (currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Global help function, returning to the needed one if there is a parameter or not.
   -- You can specify an existing command to show more details about it.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure help_command (options : in T_Substrings; parameters : in T_Substrings);
   -- Role : Clear the terminal.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure clear_command;
   
   -- Role : Function taking the arguments and spliting everything
   -- It return every options in an Unbounded_String, in a T_Substrings of options, and same for the parameters.
   -- Parameters :
   --    arguments (T_Substrings) : All the arguments
   --    options (T_Substrings) : options wanted by the user
   --    parameters (T_Substrings) : parameters entered by the user
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure get_options_parameter_and_options(arguments: in T_Substrings; options: in out T_Substrings; parameters: in out T_Substrings);
   
   -- Role : It's a recursive procedure used when the recursive ls is needed.
   -- Parameters :
   --    current_directory (T_Folder) : The current folder to display the content from
   --    preceding_path (Unbounded_String) : The path to add the directory name from.
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String);
   -- Role : Display an helping list of the know commands.
   -- Parameters :
   --    /
   -- Return :
   --    
   -- Preconditions : /
   -- Postconditions : /
   procedure help_command;
   -- Role : Display an help for a specific command.
   -- Parameters :
   --    command (String) : Name of the specified passed in parameter
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure help_command (command : in String);
   
   -- Role : Function overloading, with stop_at_penultimate at false.
   -- Function returning the folder from a path passed on parameter.
   -- If the folder does not existe, raise invalid_folder_error.
   -- The path can be relative or absolute.
   -- Parameters :
   --    original_directory (T_Folder) : Current folder
   --    path (String) : The path you want to use to get the folder from
   -- Return :
   --    T_Folder : The wanted folder, if the path is correct
   -- Preconditions : /
   -- Postconditions : /
   function go_to_folder(original_directory: in T_Folder; path: in String) return T_Folder;
   -- Role : Function returning the folder from a path passed on parameter.
   -- If the folder does not existe, raise invalid_folder_error.
   -- The path can be relative or absolute.
   -- Parameters :
   --    original_directory (T_Folder) : Current folder
   --    path (String) : The path you want to use to get the folder from
   --    stop_at_penultimate (Boolean) : If you want at the penultimate entity in path
   -- Return :
   --    T_Folder : The wanted folder, if the path is correct
   -- Preconditions : /
   -- Postconditions : /
   function go_to_folder(original_directory: in T_Folder; path: in String; stop_at_penultimate: in Boolean) return T_Folder;
   -- Role : Recursive function that calculate weigth for a folder. 
   -- It return a sum of weigth of of the folder, all its sub-folders, its fils and its sub-files.
   -- Parameters :
   --    folder (T_Folder) : The current folder to display the path from
   -- Return :
   --    Integer : The cumulated size of the folder, all its sub-folders, its fils and its sub-files.
   -- Preconditions : /
   -- Postconditions : /
   function calculate_size (folder: T_Folder) return Integer;
   -- Role : Recursive procedure copying all the countent of folder_to_copy into folder_parent_of_clone.
   -- It's then call itself for all sub-folders in folder_to_copy.
   -- Parameters :
   --    folder_to_copy (T_Folder) : Folder to duplicate content
   --    folder_parent_of_clone (T_Folder) : Folder receiving all the content
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure folder_deep_copy(folder_to_copy: T_Folder; folder_parent_of_clone: in out T_Folder);
   -- Role : Take the options parsed from the user call, and check if it exist in the handled options, passed as parameter.
   -- If an option is not supposed to be handled, then it return False. Otherwise, it returns True.
   -- Parameters :
   --    options (T_Substrings) : Options passed to test
   --    options_handled (String) : Options that are handled
   -- Return :
   --    Boolean : Indication all the call options are supported.
   -- Preconditions : /
   -- Postconditions : /
   function only_handled_options(options: in T_Substrings; options_handled: in String) return Boolean;
   
   
   -- Role : Take a path, and return the last entity from it.
   -- Parameters :
   --    path (Unbounded_String) : The path to extract the name from
   -- Return :
   --    Unbounded_String : The name from the path.
   -- Preconditions : /
   -- Postconditions : /
   function get_name_from_path(path: Unbounded_String) return Unbounded_String;
   -- Role : Check if the T_Substrings containing options, contain the specific option. 
   -- Parameters :
   --    options (T_Substrings) : The options to test from if a specific one is contained.
   --    option (Character) : The specific option to be tested.
   -- Return :
   --    Boolean : Return True if the option is countained in the options.
   -- Preconditions : /
   -- Postconditions : /
   function options_contain(options: in T_Substrings; option: in Character) return Boolean;
   
   -- sort folders & files
   type T_R_Sibling is record
      name: Unbounded_String;
      is_folder: Boolean;
   end record;
   function "<" (L, R : in T_R_Sibling) return Boolean;
   package P_Siblings_Set is new Ada.Containers.Ordered_Sets (T_R_Sibling, "<");
   subtype T_Siblings_Set is P_Siblings_Set.Set;
   
   -- Role : Create an ordered set of folders and fils names, from the directory.
   -- It return a specific set or record (T_R_Sibling), each record indicating a name and if the name correspond to a folder or a file.
   -- Parameters :
   --    directory (T_Folder) : The current folder to get the folders and files from
   -- Return :
   --    T_Siblings_Set : The ordered set of folder and files names, with a boolean indicating if its a folder or a file.
   -- Preconditions : /
   -- Postconditions : /
   function create_siblings_set (directory : in T_Folder) return T_Siblings_Set;
   -- Role : Display a set of T_R_Sibling, with color for the folder names and no color for the file name.
   -- Parameters :
   --    siblings_set (T_Siblings_Set) : The ordered set of T_R_Sibling records.
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure display_folders_and_files (siblings_set : in T_Siblings_Set);
   
end P_Commands;
