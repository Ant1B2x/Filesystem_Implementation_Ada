with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_rm is
   current_directory : T_Folder;
begin
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   run_command(current_directory, "mkdir test/ok");
   run_command(current_directory, "touch fic1");
   run_command(current_directory, "touch test/fic2");
   run_command(current_directory, "mkdir test2");
   run_command(current_directory, "mkdir test2/test2bis");
   run_command(current_directory, "touch test2/fic3");
   run_command(current_directory, "mkdir test3");
   run_command(current_directory, "touch fic4");
   
   -- rm fic1
   put_line("rm fic1");
   if has_son_with_this_name(current_directory, "fic1") then
      put_line("touch fic1 ok");
   end if;
   run_command(current_directory, "rm fic1");
   if not has_son_with_this_name(current_directory, "fic1") then
      put_line("rm fic1 ok");
   else
      put_line("rm fic1 is incoherent");
   end if;
   new_line;
   
   
   -- rm -r test/ok
   put_line("rm -r test/ok");
   if has_son_with_this_name(find_folder(current_directory, "test"), "ok") then
      put_line("mkdir test/ok ok");
   end if;
   run_command(current_directory, "rm -r test/ok");
   if not has_son_with_this_name(find_folder(current_directory, "test"), "ok") then
      put_line("rm -r test/ok ok");
   else
      put_line("rm -r test/ok is incoherent");
   end if;
   new_line;
   
   -- rm test/fic2
   put_line("rm -r test/ok");
   if has_son_with_this_name(find_folder(current_directory, "test"), "fic2") then
      put_line("touch test/fic2 ok");
   end if;
   run_command(current_directory, "rm test/fic2");
   if not has_son_with_this_name(find_folder(current_directory, "test"), "fic2") then
      put_line("rm test/fic2 ok");
   else
      put_line("rm test/fic2 is incoherent");
   end if;
   new_line;
   new_line;
   
   -- rm ./fic3 from test2
   put_line("rm ./fic3");
   current_directory := get_root;
   run_command(current_directory, "cd test2");
   if has_son_with_this_name(current_directory, "fic3") then
      put_line("touch test2/fic3 ok");
   end if;
   run_command(current_directory, "rm ./fic3");
   if not has_son_with_this_name(current_directory, "fic3") then
      put_line("rm ./fic3 from test2 ok");
   else
      put_line("rm ./fic3 from test2 is incoherent");
   end if;
   new_line;
   new_line;
   
    -- rm -r ./test2bis from test2
   put_line("rm -r ./test2bis");
   if has_son_with_this_name(current_directory, "test2bis") then
      put_line("mkdir test2/test2bis ok");
   end if;
   run_command(current_directory, "rm -r ./test2bis");
   if not has_son_with_this_name(current_directory, "test2bis") then
      put_line("rm -r ./test2bis from test2 ok");
   else
      put_line("rm -r ./test2bis from test2 is incoherent");
   end if;
   new_line;
   new_line;
   
   -- rm -r ./ from test2
   put_line("rm -r ./ from test2");
   if get_name(current_directory) = "test2" then
      put_line("mkdir test2 ok");
   end if;
   run_command(current_directory, "rm -r ./");
   if is_root(current_directory) then
      put_line("rm -r ./ from test2 ok");
   else
      put_line("rm -r ./ from test2 is incoherent");
   end if;
   new_line;
   new_line;
   
   -- rm ../fic3 from test3
   put_line("rm ../fic3 from test3");
   run_command(current_directory, "cd test3");
   if has_son_with_this_name(find_folder(current_directory, ".."), "fic3") then
      put_line("touch fic3 ok");
   end if;
   run_command(current_directory, "rm ../fic3");
   if not has_son_with_this_name(find_folder(current_directory, ".."), "fi32") then
      put_line("rm ../fic3 from test3 ok");
   else
      put_line("rm ../fic3 from test3 is incoherent");
   end if;
   new_line;
   new_line;
   
   -- rm -r ../test from test3
   put_line("rm -r ../test from test3");
   if has_son_with_this_name(find_folder(current_directory, ".."), "test") then
      put_line("mkdir test ok");
   end if;
   run_command(current_directory, "rm -r ../test");
   if not has_son_with_this_name(find_folder(current_directory, ".."), "test") then
      put_line("rm -r ../test from test3 ok");
   else
      put_line("rm -r ../test from test3 is incoherent");
   end if;
   new_line;
   new_line;
   
   
   -- rm unexisting_file
   put_line("'rm unexisting_file' with unexisting_file not existing");
   if not has_son_with_this_name(current_directory, "unexisting_file") then
      put_line("unexisting_file does not exist");
   end if;
   put_line("It should raise Invalid_File_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("A specified path is incorrect, no such file.");
   run_command(current_directory, "rm unexisting_file");
   Put_Line("============================================================");
   new_line;
   new_line;
   
   
   -- rm unexisting_folder
   put_line("'rm -r unexisting_folder' with unexisting_folder not existing");
   if not has_son_with_this_name(current_directory, "unexisting_folder") then
      put_line("unexisting_folder does not exist");
   end if;
   put_line("It should raise Invalid_Folder_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("A specified path is incorrect, no such directory.");
   run_command(current_directory, "rm -r unexisting_folder");
   Put_Line("============================================================");
   new_line;
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
end test_rm;
