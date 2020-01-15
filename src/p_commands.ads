with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;
with P_Constants; use P_Constants;
with P_Folder; use P_Folder;

package P_Commands is
   
   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type sonRecord is record
      Name: Unbounded_String;
      isFolder: Boolean;
   end record;
   
   type sons is array(1..LMAX_STRING*2) of sonRecord;
   function "<" (L, R : sonRecord) return Boolean;
   procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, sonRecord, sons);

   procedure pwdCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure lsCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure rmCommand(OptionTrue : Boolean;firstParameter: String; currentDirectory:  in out T_Folder);
   procedure pwdCommand(currentDirectory: T_Folder);
   procedure cdCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure mkdirCommand(firstParameter: String; currentDirectory: in out T_Folder);
   procedure cpCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure mvCommand(firstParameter: String; secondParameter: String; currentDirectory: T_Folder);
   procedure tarCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure touchCommand(firstParameter: String; currentDirectory: T_Folder);
   
end P_Commands;
