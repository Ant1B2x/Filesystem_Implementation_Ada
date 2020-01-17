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

   procedure pwdCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure lsCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure rmCommand(OptionTrue : Boolean;firstParameter: String; currentDirectory:  in out T_Folder);
   procedure pwdCommand(currentDirectory: T_Folder);
   procedure cdCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure mkdirCommand(firstParameter: String; currentDirectory: in out T_Folder);
   procedure cpCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure mvCommand(firstParameter: String; secondParameter: String; currentDirectory: T_Folder);
   procedure tarCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure touchCommand(firstParameter: String; currentDirectory: in out T_Folder);
   procedure help;
   procedure help_command (command : in String);
   procedure help_command (has_command : in Boolean; command : in String);
   
end P_Commands;
