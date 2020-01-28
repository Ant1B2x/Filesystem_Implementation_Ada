with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_ls is
   current_directory : T_Folder;
begin
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "touch fic1");
   run_command(current_directory, "touch test/fic2");
   run_command(current_directory, "mkdir test/ok");
   run_command(current_directory, "touch test/ok/fic3");
   run_command(current_directory, "mkdir test/ok/okbis");
   new_line;
   
   
   put_line("'ls '");
   put_line("The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("fic1  " & ASCII.ESC & "[95m" & "test  test2  " & ASCII.ESC & "[0m" );
   run_command(current_directory, "ls ");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   put_line("'ls ./'");
   put_line("The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("fic1  " & ASCII.ESC & "[95m" & "test  test2  " & ASCII.ESC & "[0m" );
   run_command(current_directory, "ls ./");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   put_line("'ls test/ok'");
   put_line("The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("fic3  " & ASCII.ESC & "[95m" & "okbis  " & ASCII.ESC & "[0m" );
   run_command(current_directory, "ls test/ok");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   put_line("'ls ../' from test");
   run_command(current_directory, " cd test");
   put_line("The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("fic1  " & ASCII.ESC & "[95m" & "test  test2  " & ASCII.ESC & "[0m" );
   run_command(current_directory, "ls ../");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   put_line("'ls .././test/./ok/./../ok/./' from test");
   run_command(current_directory, " cd test");
   put_line("The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("fic3  " & ASCII.ESC & "[95m" & "okbis  " & ASCII.ESC & "[0m" );
   run_command(current_directory, "ls .././test/./ok/./../ok/./");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- ls -f testFile
   put_line("'ls -f testFile'");
   current_directory := get_root;
   put_line("It should raise Not_Handled_Option_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Not handled option.");
   Put_line("Try help 'ls' for more information.");
   run_command(current_directory, "ls -f testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- ls testFile testFileBis
   put_line("'ls testFile testFileBis'");
   current_directory := get_root;
   put_line("It should raise Wrong_Parameters_Number_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_line("Try help 'ls' for more information.");
   run_command(current_directory, "ls testFile testFileBis");
   Put_Line("============================================================");
   new_line;
   new_line;
   
end test_ls;
