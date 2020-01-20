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
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test4");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   mkdir_command(folder, arguments);
   
   pwd_command(folder);
      
   arguments := create_substrings;
   add_substring(arguments,"test2/karibou");
   touch_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/bliblabloblu");
   touch_command(folder, arguments);
     
   arguments := create_substrings;
   add_substring(arguments,"test2/ahehohu");
   touch_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/aaaatest2");
   touch_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/bbbaaa");
   touch_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test2/test3/bababa");
   touch_command(folder, arguments);
   
   
   
   
   New_Line;
   ls_command(folder, arguments, False);
   
   New_Line;
   ls_command(folder, arguments, False);
   
   clear_command;
   
   arguments := create_substrings;
   add_substring(arguments,"test2");
   cd_command(folder, arguments);
   
   Put_Line("**** Test de tri ***");
   Put_Line("./test2:");
   ls_command(folder, arguments, True);

end test_sort;
