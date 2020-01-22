with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_cp is
   folder : T_Folder;
   arguments: T_Substrings;
begin

   folder := get_root;
   
   put(ESC & "[31m" & get_pwd(folder) & " > " & ESC & "[0m");
   
   arguments := create_substrings;
   add_substring(arguments,"testCP");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP/test1");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP/Blou");
   touch_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP/test1/hum");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP/test2");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP/test2/AH");
   mkdir_command(folder, arguments);
   
   arguments := create_substrings;
   ls_command(folder, arguments, True);
   
   arguments := create_substrings;
   add_substring(arguments,"testCP");
   add_substring(arguments,"test");
   cp_command(folder, arguments, True);
   Put_Line("Copie du testCP dans test :");
   arguments := create_substrings;
   
   ls_command(folder, arguments, True);
   
   Put_Line("Suppression de : testCP/Blou");
   arguments := create_substrings;
   add_substring(arguments,"testCP/Blou");
   rm_command(folder, arguments, False);
   
   arguments := create_substrings;
   ls_command(folder, arguments, True);
   
   
   Put_Line("Suppression de : test/test2/AH");
   arguments := create_substrings;
   add_substring(arguments,"test/test2/AH");
   rm_command(folder, arguments, True);
   
   arguments := create_substrings;
   ls_command(folder, arguments, True);
   
   Put_Line("Copie de : test/Blou dans test/test2");
   arguments := create_substrings;
   add_substring(arguments,"test/Blou");
   add_substring(arguments,"test/test2/Blou");
   cp_command(folder, arguments, False);
   
   arguments := create_substrings;
   ls_command(folder, arguments, True);
   
   Put_Line("Suppression de : test/test2/Blou");
   arguments := create_substrings;
   add_substring(arguments,"test/test2/Blou");
   rm_command(folder, arguments, False);
   
   arguments := create_substrings;
   add_substring(arguments,"test");
   ls_command(folder, arguments, True);
   
   Put_Line("mv de : test/Blou test/test2/Blou");
   arguments := create_substrings;
   add_substring(arguments,"test/Blou");
   add_substring(arguments,"test/test2/Blou");
   mv_command(folder, arguments);
   
   arguments := create_substrings;
   add_substring(arguments,"test");
   ls_command(folder, arguments, True);
   
end test_cp;
