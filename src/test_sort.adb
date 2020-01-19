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
   arguments: T_Substrings;
begin

   folder := get_root;
   
   put(ESC & "[31m" & get_pwd(folder) & " > " & ESC & "[0m");
   
   arguments := create_substrings;
   add_substring(arguments,"test1");
   mkdirCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdirCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test4");
   mkdirCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3");
   mkdirCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdirCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdirCommand(arguments, folder);
   
   pwd_command(folder);
      
   arguments := create_substrings;
   add_substring(arguments,"test2/karibou");
   touchCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/bliblabloblu");
   touchCommand(arguments, folder);
     
   arguments := create_substrings;
   add_substring(arguments,"test2/ahehohu");
   touchCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/aaaatest2");
   touchCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/bbbaaa");
   touchCommand(arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/bababa");
   touchCommand(arguments, folder);
   
   
   
   
   New_Line;
   ls_command(folder, arguments, False);
   
   New_Line;
   ls_command(folder, arguments, False);
   
   clear_command;
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   cdCommand(arguments,folder);
   
   Put_Line("**** Test de tri ***");
   Put_Line("./test2:");
   -- ce fichier sera delete de toute façon
   --display_folders_and_files(create_set(folder));

end test_sort;
