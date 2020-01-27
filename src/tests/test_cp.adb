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
   parameters: T_Substrings;
   options: T_Substrings;
begin

   folder := get_root;
   
   put(ESC & "[31m" & get_pwd(folder) & " > " & ESC & "[0m");
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP");
   --mkdir_command(folder, options, parameters);
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/test1");
   --mkdir_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/Blou");
   --touch_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test");
   --mkdir_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/test1/hum");
   --mkdir_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/test2");
   --mkdir_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/test2/AH");
   --mkdir_command(folder, options, parameters);
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   
   Put_Line("Copie du testCP dans test :");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,"testCP");
   --add_substring(parameters,"test/test");
   --cp_command(folder, options, parameters);
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   Put_Line("Suppression de : testCP/Blou");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"testCP/Blou");
   --rm_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   
   Put_Line("Suppression de : test/test/test2/AH");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,"test/test/test2/AH");
   --rm_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   Put_Line("Copie de : test/test/Blou dans test/test/test2");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test/test/Blou");
   --add_substring(parameters,"test/test/test2/Blou");
   --cp_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   Put_Line("Suppression de : test/test/test2/Blou");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test/test/test2/Blou");
   --rm_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
   Put_Line("mv de : test/test/Blou test/test/test2/Blou");
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(parameters,"test/test/Blou");
   --add_substring(parameters,"test/test/test2/Blou");
   --mv_command(folder, options, parameters);
   
   parameters := create_substrings;
   options := create_substrings;
   --add_substring(options,"r");
   --add_substring(parameters,".");
   --ls_command(folder, options, parameters);
   
end test_cp;
