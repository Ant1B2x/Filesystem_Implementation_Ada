with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
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

   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch, help, clear);

   function get_pwd (current_directory : in T_Folder) return String;
   procedure pwd_command (current_directory: in T_Folder);
   procedure ls_command (current_directory : in T_Folder; arguments : in T_Substrings; recursive : in Boolean);
   procedure rm_command (current_directory : in out T_Folder; arguments : in T_Substrings; recursive : in Boolean);
   
   procedure cd_command (currentDirectory: in out T_Folder; arguments: T_Substrings);
   procedure mkdir_command (currentDirectory: in out T_Folder; arguments: T_Substrings);
   procedure cp_command (currentDirectory: in out T_Folder; arguments: T_Substrings; OptionTrue : Boolean);
   procedure mv_command (currentDirectory: in out T_Folder;arguments: T_Substrings);
   procedure tar_command (currentDirectory: in out T_Folder;arguments: T_Substrings);
   procedure touch_command (currentDirectory: in out T_Folder;arguments: T_Substrings);
   
   procedure help_command (arguments : in T_Substrings);
   procedure clear_command;

private
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String);
   procedure help_command;
   procedure help_command (command : in String);
   
   function go_to_folder(original_directory: in T_Folder; path: in String) return T_Folder;
   function go_to_folder(original_directory: in T_Folder; path: in String; stop_at_penultimate: in Boolean) return T_Folder;
   function calculate_size (folder: T_Folder) return Integer;
   procedure folder_deep_copy(folder_to_copy: T_Folder; folder_parent_of_clone: in out T_Folder);
   
   -- sort folders & files
   type T_R_Sibling is record
      name: Unbounded_String;
      is_folder: Boolean;
   end record;
   function "<" (L, R : in T_R_Sibling) return Boolean;
   package P_Siblings_Set is new Ada.Containers.Ordered_Sets (T_R_Sibling, "<");
   subtype T_Siblings_Set is P_Siblings_Set.Set;
   
   function create_siblings_set (directory : in T_Folder) return T_Siblings_Set;
   procedure display_folders_and_files (siblings_set : in T_Siblings_Set);
   
end P_Commands;
