with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_File; use P_File;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;
with P_Substrings; use P_Substrings;

package P_Commands is
   
   wrong_number_of_arguments: Exception;

   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch, help);
   
   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type sonRecord is record
      Name: Unbounded_String;
      isFolder: Boolean;
   end record;
   
   type sons is array(Natural range <>) of sonRecord;
   function "<" (L, R : sonRecord) return Boolean;
   package Composite_Sets is new Ada.Containers.Ordered_Sets (sonRecord);
   procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, sonRecord, sons);
   procedure Sort2 is new Ada.Containers.Generic_Array_Sort(Natural, sonRecord, sons, "<");

   procedure pwdCommand(arguments: T_Substrings; currentDirectory: T_Folder);
   procedure lsCommand(OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder);
   procedure rmCommand(OptionTrue : Boolean;arguments: T_Substrings; currentDirectory:  in out T_Folder);
   procedure pwdCommand(currentDirectory: T_Folder);
   procedure cdCommand(arguments: T_Substrings; currentDirectory: T_Folder);
   procedure mkdirCommand(arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure cpCommand(OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder);
   procedure mvCommand(arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure tarCommand(arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure touchCommand(arguments: T_Substrings; currentDirectory: in out T_Folder);
   procedure help_command;
   procedure help_command (command : in String);
   procedure help_command (has_command : in Boolean; command : in String);
   function calculate_size(folder: T_Folder) return Integer;

private
   function go_to_folder(original_directory: in T_Folder; path: in String) return T_Folder;
   function go_to_folder(original_directory: in T_Folder; path: in String; stop_at_penultimate: in Boolean) return T_Folder;
   procedure folder_deep_copy(folder_to_copy: T_Folder; folder_parent_of_clone: in out T_Folder);
   
end P_Commands;
