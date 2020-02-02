with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_cp is
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
   
   -- cp test/ok/fic3 test/fic3" 
   put_line("cp test/ok/fic3 test/fic3");
   if not has_son_with_this_name(find_folder(current_directory, "test"), "fic3") then
      put_line(ASCII.ESC & "[92m" & "not fic3 in test" & ASCII.ESC & "[0m");
   end if;
   run_command(current_directory, "cp test/ok/fic3 test/fic3");
   if has_son_with_this_name(find_folder(current_directory, "test"), "fic3") then
      put_line(ASCII.ESC & "[92m" & "cp test/ok/fic3 test/fic3 ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "cp test/ok/fic3 test/fic3 is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   
   -- cp -r test/ok/okbis test2/okbis" 
   put_line("cp -r test/ok/okbis test2/okbis");
   if not has_son_with_this_name(find_folder(current_directory, "test2"), "okbis") then
      put_line(ASCII.ESC & "[92m" & "not okbis in test2" & ASCII.ESC & "[0m");
   end if;
   run_command(current_directory, "cp -r test/ok/okbis test2/okbis");
   if has_son_with_this_name(find_folder(current_directory, "test2"), "okbis") then
      put_line(ASCII.ESC & "[92m" & "cp -r test/ok/okbis test2/okbis ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "cp -r test/ok/okbis test2/okbis is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   -- cp -r test/ok test2/ok" 
   put_line("cp -r test/ok test2/ok");
   if not has_son_with_this_name(find_folder(current_directory, "test2"), "ok") then
      put_line(ASCII.ESC & "[92m" & "no ok folder in test2" & ASCII.ESC & "[0m");
   end if;
   run_command(current_directory, "cp -r test/ok test2/ok");
   if has_son_with_this_name(find_folder(current_directory, "test2"), "ok") and  
     has_son_with_this_name(find_folder(find_folder(current_directory, "test2"), "ok"), "fic3") and 
     has_son_with_this_name(find_folder(find_folder(current_directory, "test2"), "ok"), "okbis") then
      put_line(ASCII.ESC & "[92m" & "cp -r test/ok test2/ok ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "cp -r test/ok test2/ok is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   -- cp ../fic3 ./fic3 from test2/okbis
   put_line("cp ../ok/fic3 ./fic3 from test2/okbis");
   current_directory := get_root;
   run_command(current_directory, "cd test2/okbis");
   if not has_son_with_this_name(current_directory, "fic3") then
      put_line(ASCII.ESC & "[92m" & "no fic3 in test2/okbis" & ASCII.ESC & "[0m");
   end if;
   run_command(current_directory, "cp ../ok/fic3 ./fic3");
   if has_son_with_this_name(current_directory, "fic3") then
      put_line(ASCII.ESC & "[92m" & "cp ../ok/fic3 ./fic3 from test2/okbis ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "cp ../ok/fic3 ./fic3 from test2/okbis is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   -- cp -r ../ok ./ok from test2/okbis
   put_line("cp -r ../ok ./ok from test2/okbis");
   if not has_son_with_this_name(current_directory, "ok") then
      put_line(ASCII.ESC & "[92m" & "no ok in test2/okbis" & ASCII.ESC & "[0m");
   end if;
   run_command(current_directory, "cp -r ../ok ./ok");
   if has_son_with_this_name(current_directory, "ok") and
     has_son_with_this_name(find_folder(current_directory, "ok"), "fic3") and
     has_son_with_this_name(find_folder(current_directory, "ok"), "okbis") then
      put_line(ASCII.ESC & "[92m" & "cp -r ../ok ./ok from test2/okbis ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "cp -r ../ok ./ok from test2/okbis is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   current_directory := get_root;
   -- cp unexisting_file
   put_line("'cp unexisting_file test/unexisting_file' with unexisting_file not existing");
   if not has_son_with_this_name(current_directory, "unexisting_file") then
      put_line(ASCII.ESC & "[92m" & "unexisting_file does not exist" & ASCII.ESC & "[0m");
   end if;
   put_line("It should raise Constraint_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("A specified path is incorrect, no such file.");
   run_command(current_directory, "cp unexisting_file test2/unexisting_file");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   -- cp unexisting_folder
   put_line("'cp -r unexisting_folder test/unexisting_folder' with unexisting_folder not existing");
   if not has_son_with_this_name(current_directory, "unexisting_folder") then
      put_line(ASCII.ESC & "[92m" & "unexisting_folder does not exist" & ASCII.ESC & "[0m");
   end if;
   put_line("It should raise Constraint_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("A specified path is incorrect, no such directory.");
   run_command(current_directory, "cp -r unexisting_folder test2/unexisting_folder");
   Put_Line("============================================================");
   new_line;
   
   
   
   -- cp 
   put_line("'cp '");
   put_line("It should raise Constraint_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_Line("Try help 'cp' for more information.");
   run_command(current_directory, "cp ");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   
   -- cp -r 
   put_line("'cp -r '");
   put_line("It should raise Constraint_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_Line("Try help 'cp' for more information.");
   run_command(current_directory, "cp -r ");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- cp -r test test/ok
   put_line("cp -r test test/ok");
   put_line("It should raise Copy_Into_Itself_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Cannot copy a directory into itself.");
   run_command(current_directory, "cp -r test test/test ");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- cp -f testFile
   put_line("'cp -f testFile'");
   current_directory := get_root;
   put_line("It should raise Not_Handled_Option_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Not handled option.");
   Put_line("Try help 'cp' for more information.");
   run_command(current_directory, "cp -f testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- cp testFile
   put_line("'cp testFile'");
   current_directory := get_root;
   put_line("It should raise Wrong_Parameters_Number_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_line("Try help 'cp' for more information.");
   run_command(current_directory, "cp testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   
end test_cp;
