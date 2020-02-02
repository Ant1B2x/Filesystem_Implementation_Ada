with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_cd is
   current_directory : T_Folder;
begin
   
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "mkdir test/ok");
   
   -- cd .. in root
   put_line("cd .. in root");
   current_directory := get_root;
   run_command(current_directory, "cd ..");
   if is_root(current_directory) then
      put_line(ASCII.ESC & "[92m" & "is_root(current_directory)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_root(current_directory) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd . in root
   put_line("cd . in root");
   current_directory := get_root;
   run_command(current_directory, "cd .");
   if is_root(current_directory) then
      put_line(ASCII.ESC & "[92m" & "is_root(current_directory)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_root(current_directory) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd test
   put_line("cd test in root");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   if get_name(current_directory) = "test" then
      put_line(ASCII.ESC & "[92m" & "get_name(current_directory) = 'test'" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(current_directory) = 'test' is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd .. from test
   put_line("cd .. from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ..");
   if is_root(current_directory) then
      put_line(ASCII.ESC & "[92m" & "is_root(current_directory)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_root(current_directory) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd ../test2 from test
   put_line("cd ../test2 from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ../test2");
   if get_name(current_directory) = "test2" then
      put_line(ASCII.ESC & "[92m" & "get_name(current_directory) = 'test2'" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(current_directory) = 'test2' is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd ./ from test
   put_line("cd ./ from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ./");
   if get_name(current_directory) = "test" then
      put_line(ASCII.ESC & "[92m" & "get_name(current_directory) = 'test'" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(current_directory) = 'test' is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- cd ./.././test/./ok from test2
   put_line("cd ./.././test/./ok from test2");
   current_directory := get_root;
   run_command(current_directory, "cd test2");
   run_command(current_directory, "cd ./.././test/./ok");
   if get_name(current_directory) = "ok" then
      put_line(ASCII.ESC & "[92m" & "get_name(current_directory) = 'ok'" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(current_directory) = 'ok' is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   -- cd unexisting_folder
   put_line("'cd unexisting_folder' with unexisting_folder not existing");
   current_directory := get_root;
   if not has_son_with_this_name(current_directory, "unexisting_folder") then
      put_line(ASCII.ESC & "[92m" & "unexisting_folder does not exist" & ASCII.ESC & "[0m");
   end if;
   put_line("It should raise Invalid_Folder_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("A specified path is incorrect, no such directory");
   run_command(current_directory, "cd unexisting_folder");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   -- mkdir -f testFile
   put_line("'mkdir -f testFile'");
   current_directory := get_root;
   put_line("It should raise Not_Handled_Option_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Not handled option.");
   Put_line("Try help 'mkdir' for more information.");
   run_command(current_directory, "mkdir -f testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- mkdir testFile testFileBis
   put_line("'mkdir testFile testFileBis'");
   current_directory := get_root;
   put_line("It should raise Wrong_Parameters_Number_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_line("Try help 'mkdir' for more information.");
   run_command(current_directory, "mkdir testFile testFileBis");
   Put_Line("============================================================");
   new_line;
   new_line;
   
end test_cd;
