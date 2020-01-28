with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_tar is
   current_directory : T_Folder;
begin
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "mkdir test3");
   run_command(current_directory, "touch fic1");
   run_command(current_directory, "touch test/fic2");
   run_command(current_directory, "mkdir test/ok");
   run_command(current_directory, "touch test/ok/fic3");
   run_command(current_directory, "mkdir test/ok/okbis");
   new_line;
   
   
   -- tar test
   put_line("tar test");
   current_directory := get_root;
   run_command(current_directory, "tar test");
   if has_son_with_this_name(current_directory, "test.tar") then
      put_line("tar test is ok");
   else
      put_line("tar test is incoherent");
   end if;
   new_line;
   
   -- tar ../test from test2
   put_line("tar ../test from test2");
   current_directory := get_root;
   run_command(current_directory, "cd test2");
   run_command(current_directory, "tar ../test");
   if has_son_with_this_name(current_directory, "test.tar") then
      put_line("tar ../test from test2 is ok");
   else
      put_line("tar ../test from test2 is incoherent");
   end if;
   new_line;
   
   -- tar ./ from test3
   put_line("tar ./ from test3");
   current_directory := get_root;
   run_command(current_directory, "cd test3");
   run_command(current_directory, "tar ./");
   if has_son_with_this_name(current_directory, "test3.tar") then
      put_line("tar ./ from test3 is ok");
   else
      put_line("tar ./ from test3 is incoherent");
   end if;
   new_line;
   new_line;
   
   -- test3/test3.tar has the good size
   put_line("test3/test3.tar has the good size (10'000)");
   current_directory := get_root;
   run_command(current_directory, "cd test3");
   if get_size(find_file(current_directory, "test3.tar")) = 10000 then
      put_line("test3/test3.tar has the expected size");
      put_line("Size : " & get_size(find_file(current_directory, "test3.tar"))'Image);
   else
      put_line("size of test3/tets3.tar is incoherent");
   end if;
   new_line;
   new_line;
   
   
   -- test3/test3.tar has the good size
   put_line("test.tar has the good size (30'000)");
   current_directory := get_root;
   if get_size(find_file(current_directory, "test.tar")) = 30000 then
      put_line("test.tar has the expected size");
      put_line("Size : " & get_size(find_file(current_directory, "test.tar"))'Image);
   else
      put_line("size of test.tar is incoherent");
   end if;
   run_command(current_directory, "cd ..");
   new_line;
   new_line;
   
   
   -- tar -f testFile
   put_line("'tar -f testFile'");
   current_directory := get_root;
   put_line("It should raise Not_Handled_Option_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Not handled option.");
   Put_line("Try help 'tar' for more information.");
   run_command(current_directory, "tar -f testFile");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   -- tar testFile testFileBis
   put_line("'tar testFile testFileBis'");
   current_directory := get_root;
   put_line("It should raise Wrong_Parameters_Number_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("Wrong number of parameters.");
   Put_line("Try help 'tar' for more information.");
   run_command(current_directory, "tar testFile testFileBis");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   
end test_tar;
