with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_sort is
   folder : T_Folder;
   parameters: T_Substrings;
   options: T_Substrings;
begin

   folder := get_root;
   
   put(ESC & "[31m" & get_pwd(folder) & " > " & ESC & "[0m");
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test1");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/test4");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/test3");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2");
   --mkdir_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --pwd_command(folder, parameters, options);
      
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/karibou");
   --touch_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/bliblabloblu");
   --touch_command(folder, parameters, options);
     
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/ahehohu");
   --touch_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/test3/aaaatest2");
   --touch_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/test3/bbbaaa");
   --touch_command(folder, parameters, options);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2/test3/bababa");
   --touch_command(folder, parameters, options);
   
   
   
   
   New_Line;
   parameters := create_substrings;
   options := create_substrings;
   --ls_command(folder, parameters, options);
   
   New_Line;
   --ls_command(folder, parameters, options);
   
   --clear_command;
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test2");
   --cd_command(folder, parameters, options);
   
   Put_Line("**** Test de tri ***");
   Put_Line("./test2:");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"-r");
   --ls_command(folder, parameters, options);

end test_sort;
