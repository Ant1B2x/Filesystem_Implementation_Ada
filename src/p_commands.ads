with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Metadata; use P_Metadata;
with P_File; use P_File;
with P_Folder; use P_Folder;

package P_Commands is
   
   -- Raised when a specified folder is invalid
   Invalid_Folder_Error : Exception;
   -- Raised when a specified file is invalid
   Invalid_File_Error : Exception;
   -- Raised when trying to copy a folder into itself
   Copy_Into_Itself_Error : Exception;
   -- Raised when an option only contains '-' character
   Empty_Option_Error : Exception;
   -- Raised when a command receives an option that it can't use
   Not_Handled_Option_Error : Exception;
   -- Raised when a command receives too many or too few parameters
   Wrong_Parameters_Number_Error : Exception;

   -- #################################################### PUBLIC SUBROUTINES ############################################################
   
   -- Enumeration type of different commands available
   type E_Encoded_Commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch, help, clear);
   
   -- Role: Return the string representation of an E_Encoded_Commands
   -- Parameters:
   --    encoded_command (in E_Encoded_Commands) : Command that the image will be returned
   -- Return:
   --    return (String) : Image of the E_Encoded_Commands
   -- Preconditions : /
   -- Postconditions : /
   function command_to_string (encoded_command : in E_Encoded_Commands) return String;
   
   -- Role : Decode a command line and execute a command with the correct options and parameters
   -- Parameters :
   --    current_directory (in out T_Folder) : Folder where we are currently in, should be in "in / out" for commands such as "touch", "mkdir"
   --    command_line (in String) : Command line to execute
   -- Return : /
   -- Preconditions : the command line length is inferior or equal to the maximum length of a string
   -- Postconditions : /
   procedure run_command (current_directory : in out T_Folder; command_line : in String)
     with Pre => command_line'length <= LMAX_STRING;
   
private
   
   -- ############################################ VARIOUS PRIVATE SUBROUTINES ###########################################################
   
   -- Role : Function taking the arguments and spliting it to options ("-r", etc...) and parameters (path, etc...)
   -- It returns a T_Substrings of options and a T_Substrings of parameters
   -- Parameters :
   --    arguments (in T_Substrings) : All the arguments entered by the user
   --    options (in out T_Substrings) : Options wanted by the user
   --    parameters (in out T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure get_options_and_parameters (arguments : in T_Substrings; options : in out T_Substrings; parameters : in out T_Substrings);
   
   -- Role : Split a string of options (even complexes like "-rf -as")
   -- It returns a T_Substrings of each separated options (like ["r","f","a","s"])
   -- Parameters :
   --    options_as_string (in String) : Options to split
   -- Return :
   --    T_Substrings : A T_Substrings containing each option separately
   -- Preconditions : /
   -- Postconditions : /
   function split_options(options_as_string : in String) return T_Substrings;
   
   -- Role : Check if the T_Substrings containing options, contains the specific option
   -- Parameters :
   --    options (in T_Substrings) : The options to test if a specific one is contained
   --    option (in Character) : The specific option to be tested
   -- Return :
   --    Boolean : True if the option is contained in the options, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function contains_option (options : in T_Substrings; option : in Character) return Boolean;
   
   -- Role : Take the options parsed from the user call, and check if they all exist in handled options
   -- If an option is not supposed to be handled, then it returns False, otherwise, it returns True
   -- Parameters :
   --    options (in T_Substrings) : Options passed to test
   --    handled_options (in String) : Options that are handled
   -- Return :
   --    Boolean : True if all options are in the handled_options, False otherwise
   -- Preconditions : /
   -- Postconditions : /
   function only_handled_options(options : in T_Substrings; handled_options : in String) return Boolean;
   
   -- Role : Function returning the folder from a path passed as parameter
   -- The path can be relative to original_directory or absolute
   -- Parameters :
   --    original_directory (in T_Folder) : Original / current folder
   --    path (in String) : The path of the folder you want
   --    stop_at_penultimate (in Boolean) := False : If you want to stop at the penultimate entity in path, false by default
   -- Return :
   --    T_Folder : The wanted folder, if the path is correct
   -- Preconditions : /
   -- Postconditions : /
   function go_to_folder (original_directory : in T_Folder; path : in String; stop_at_penultimate : in Boolean := False) return T_Folder;
   
   -- Role : Take a path, and return the last entity from it
   -- Parameters :
   --    path (in Unbounded_String) : The path to extract the name from
   -- Return :
   --    Unbounded_String : The name (last entity) from the path
   -- Preconditions : /
   -- Postconditions : /
   function get_name_from_path (path : in Unbounded_String) return Unbounded_String;
   
   -- Role : Recursively calculate the size of a folder
   -- It returns a sum of weigth of the folder, all its sub-folders, its files and its sub-files
   -- Parameters :
   --    current_directory (in T_Folder) : The folder that we want to calculate size
   -- Return :
   --    Integer : The cumulated size of the folder, all its sub-folders, its files and its sub-files
   -- Preconditions : /
   -- Postconditions : /
   function calculate_size (current_directory : in T_Folder) return Integer;
   
   -- Represents a folder sibling, file or folder, used to sort them alphabetically
   type T_R_Sibling is record
      name: Unbounded_String;
      is_folder: Boolean;
   end record;
   
   -- Role : Compare two siblings, file or folder, used to sort them alphabetically
   -- Parameters:
   --    sibling1 (in T_R_Sibling) : First sibling to compare
   --    sibling2 (in T_R_Sibling) : Second sibling to compare
   -- Return
   --    True if sibling1 name is inferior to sibling2 name, else return False 
   -- Preconditions : /
   -- Postconditions : /
   function compare_T_R_Siblings (sibling1, sibling2 : in T_R_Sibling) return Boolean;
   package P_Siblings_Set is new Ada.Containers.Ordered_Sets (T_R_Sibling, compare_T_R_Siblings);
   subtype T_Siblings_Set is P_Siblings_Set.Set;
   
   -- Role : Create an ordered set of folder and file names, from a directory
   -- It returns a specific set of records (T_R_Sibling), each record indicating a name and if the name correspond to a folder or a file
   -- Parameters :
   --    current_directory (in T_Folder) : The current folder to get the folders and files from
   -- Return :
   --    T_Siblings_Set : An ordered set of folder and file names from current_directory
   -- Preconditions : /
   -- Postconditions : /
   function create_siblings_set (current_directory : in T_Folder) return T_Siblings_Set;
   
   -- Role : Display a set of T_R_Sibling, with color for folder names and no color for file names
   -- Parameters :
   --    siblings_set (in T_Siblings_Set) : The ordered set of T_R_Sibling records
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure display_folders_and_files (siblings_set : in T_Siblings_Set);
   
   -- Role : Recursive procedure copying all the content from a folder to another
   -- It's then called recursive for all sub-folders in folder1
   -- Parameters :
   --    folder1 (in T_Folder) : Folder from which duplicate content
   --    folder2 (in out T_Folder) : Folder receiving all the content
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure folder_deep_copy (source_folder : in T_Folder; destination_folder : in out T_Folder);
   
   -- Role : Display an help list of known commands
   -- Parameters : /
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure print_global_help;
   
   -- Role : Display an help for a specific command
   -- Parameters :
   --    command (in String) : Name of the specific command that we want the help
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure print_specific_help (command : in String);
   
   -- ################################################ PRIVATE COMMANDS ##################################################################
   
   -- Role : Print the pwd (path+name) of the current directory
   -- Parameters :
   --    current_directory (in T_Folder) : The current directory to display the pwd from
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure pwd_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Change the current folder, to move in the tree of folders
   -- Can only be used for existing folders, by absolute or relative path as parameter
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure cd_command (current_directory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Print the folders and files inside the current folder, sort it alphabetically
   -- Parameters :
   --    current_directory (in T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user, (ex "-r" for recursive ls)
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure ls_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Recursive procedure called when the recursive ls is needed
   -- Parameters :
   --    current_directory (in T_Folder) : The current folder to display the content from
   --    preceding_path (in Unbounded_String) : The path to add the directory name from (for display)
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String);
   
   -- Role : Create the wanted directory, via an absolute or relative path
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure mkdir_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Create a new file, needs a path with file name as last entity
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return /
   -- Preconditions : /
   -- Postconditions : /
   procedure touch_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Copy a file , or a directory with "-r" option
   -- Use 2 parameters (source path and destination path), and you have to specify the name of the new copy 
   -- Copy all the content of the first parameter into a new folder, with the given new name, in the given path
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure cp_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Move a file, or a directory with "-r" option
   -- Use 2 parameters (source path and destination path), and you have to specify the name of the new copy
   -- Move all the content of the first parameter into a new folder, with the given new name, in the given path
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure mv_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Delete a file, or a folder with "-r" option
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user, (ex "-r" for directory remove)
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure rm_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Create a .tar file with the name of the folder you want to compress
   -- The weight of this archive is the sum of all folder, file, sub-folders and sub-files of the specified folder
   -- Parameters :
   --    current_directory (in out T_Folder) : The current folder
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure tar_command (current_directory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings);
   
   -- Role : Clear the terminal
   -- Parameters : /
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure clear_command;
   
   -- Role : Help command, calling global or specific help printers depending on the parameters
   -- You can specify an existing command to show more details about it
   -- Parameters :
   --    options (in T_Substrings) : Options wanted by the user
   --    parameters (in T_Substrings) : Parameters entered by the user
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure help_command (options : in T_Substrings; parameters : in T_Substrings);
   
end P_Commands;
