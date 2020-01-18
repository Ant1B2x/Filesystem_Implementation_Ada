with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;

with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_File; use P_File;
with P_Substrings; use P_Substrings;

package P_Commands is
   
   Wrong_Arguments_Number_Error : Exception;

   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch, help);

   function get_pwd (current_directory : in T_Folder) return String;
   procedure pwdCommand (currentDirectory: T_Folder);
   procedure lsCommand (OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder);
   procedure rmCommand (OptionTrue : Boolean;arguments: T_Substrings; currentDirectory:  in out T_Folder);
   procedure cdCommand (arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure mkdirCommand (arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure cpCommand (OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder);
   procedure mvCommand (arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure tarCommand (arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure touchCommand (arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure help_command (arguments : in T_Substrings);
   function calculate_size (folder: T_Folder) return Integer;
   
   
   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type T_R_Sibling is record
      name: Unbounded_String;
      is_folder: Boolean;
   end record;
   type T_Sibling_Records is array (Natural range <>) of T_R_Sibling;
   function "<" (L, R : in T_R_Sibling) return Boolean;
   package Composite_Sets is new Ada.Containers.Ordered_Sets (T_R_Sibling);
   procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, T_R_Sibling, T_Sibling_Records);
   procedure Sort2 is new Ada.Containers.Generic_Array_Sort(Natural, T_R_Sibling, T_Sibling_Records, "<");
   function get_folders_and_files(folder: T_Folder) return T_Sibling_Records;

private
   procedure help_command;
   procedure help_command (command : in String);
   
   function go_to_folder(original_directory: in T_Folder; path: in String) return T_Folder;
   function go_to_folder(original_directory: in T_Folder; path: in String; stop_at_penultimate: in Boolean) return T_Folder;
   procedure folder_deep_copy(folder_to_copy: T_Folder; folder_parent_of_clone: in out T_Folder);
   

   
   
   
end P_Commands;
