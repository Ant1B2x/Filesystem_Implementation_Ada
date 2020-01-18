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
   command : String(1..LMAX_STRING);
   command_length : Integer;
   arguments: T_Substrings;
   allSons: T_Sibling_Records(1..2*LMAX_STRING);
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
   
   pwdCommand(folder);
      
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
   lsCommand(False, arguments, folder);
   
   New_Line;
   lsCommand(True, arguments, folder);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   cdCommand(arguments,folder);
   
   allSons := get_folders_and_files(folder);
   New_Line;
   New_Line;
   New_Line;
   Put_Line("**** Test de tri ***");
   Put_Line("./test2:");
   for i in 1..allSons'Last loop
      if(allSons(i).is_folder)then
         Put(ESC & "[36m" & To_String(allSons(i).name) & ESC & "[0m" & "  ");
      else
         Put(To_String(allSons(i).name) & "  ");
      end if;
   end loop;


end test_sort;
