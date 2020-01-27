with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_pwd is
   current_directory : T_Folder;
begin
   
   New_Line;
   Put_Line("The couple of lines between two '===...===' should be the same :");
   New_Line;
   
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "mkdir test/ok");
   
   -- pwd in root
   put_line("pwd in root");
   current_directory := get_root;
   run_command(current_directory, "cd ..");
   Put_Line("============================================================");
   
   Put_Line("/");
   run_command(current_directory, "pwd");
   Put_Line("============================================================");
   new_line;
   New_Line;
   
   -- pwd in test
   put_line("pwd in test");
   current_directory := get_root;
   run_command(current_directory, "cd ..");
   Put_Line("============================================================");
   run_command(current_directory, "cd test");
   Put_Line("/test");
   run_command(current_directory, "pwd");
   Put_Line("============================================================");
   new_line;
   New_Line;
   
   -- pwd in test/ok
   put_line("pwd in test/ok");
   current_directory := get_root;
   run_command(current_directory, "cd ..");
   Put_Line("============================================================");
   run_command(current_directory, "cd test/ok");
   Put_Line("/test/ok");
   run_command(current_directory, "pwd");
   Put_Line("============================================================");
   new_line;
   New_Line;
end test_pwd;
