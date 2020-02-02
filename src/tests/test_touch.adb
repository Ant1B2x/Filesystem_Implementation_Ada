with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_touch is
   current_directory : T_Folder;
begin
   
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "mkdir test3");
   run_command(current_directory, "mkdir test/ok");
   
   
   -- touch testFile in root
   put_line("'touch testFile'");
   current_directory := get_root;
   run_command(current_directory, "touch testFile");
   if has_son_with_this_name(current_directory, "testFile") then
      put_line(ASCII.ESC & "[92m" & "touch testFile ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "touch testFile is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- touch test/okFile in root
   put_line("'touch test/okFile'");
   run_command(current_directory, "touch test/okFile");
   if has_son_with_this_name(find_folder(current_directory, "test"), "okFile") then
      put_line(ASCII.ESC & "[92m" & "touch test/okFile ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "touch test/okFile is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- touch ./testFile2 in root
   put_line("'touch ./testFile2'");
   run_command(current_directory, "touch ./testFile2");
   if has_son_with_this_name(current_directory, "testFile2") then
      put_line(ASCII.ESC & "[92m" & "touch ./testFile2 ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "touch ./testFile2 is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- touch ../testFile3 from test
   put_line("'touch ../testFile3' from test");
   current_directory := find_folder(current_directory, "test");
   run_command(current_directory, "touch ../testFile3");
   if has_son_with_this_name(get_root, "testFile3") then
      put_line(ASCII.ESC & "[92m" & "touch ../testFile3 ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "touch ../testFile3 is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   
   -- touch /test3/okFile from test
   put_line("'touch /test3/okFile' from test");
   current_directory := get_root;
   current_directory := find_folder(current_directory, "test");
   run_command(current_directory, "touch /test3/okFile");
   if has_son_with_this_name(find_folder(get_root, "test3"), "okFile") then
      put_line(ASCII.ESC & "[92m" & "touch /test3/okFile ok" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "touch /test3/okFile is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   new_line;
   
   -- touch testFile with testFile already existing
   put_line("'touch testFile' with testFile already existing");
   current_directory := get_root;
   run_command(current_directory, "touch testFile");
   put_line("It should raise Same_Name_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Cannot create file or directory: A file or directory with same name already exists");
   run_command(current_directory, "touch testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- touch -r testFile
   put_line("'touch -r testFile'");
   current_directory := get_root;
   put_line("It should raise Not_Handled_Option_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Not handled option.");
   Put_line("Try help 'touch' for more information.");
   run_command(current_directory, "touch -r testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- touch testFile testFileBis
   put_line("'touch testFile testFileBis'");
   current_directory := get_root;
   put_line("It should raise Wrong_Parameters_Number_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_line("Try help 'touch' for more information.");
   run_command(current_directory, "touch testFile testFileBis");
   Put_Line("============================================================");
   new_line;
   new_line;
   
end test_touch;
