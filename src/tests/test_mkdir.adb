with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_mkdir is
   current_directory : T_Folder;
begin
   -- mkdir test in root
   put_line("'mkdir test'");
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   if has_son_with_this_name(current_directory, "test") then
      put_line("mkdir test ok");
   else
      put_line("mkdir test is incoherent");
   end if;
   new_line;
   
   -- mkdir test/ok in root
   put_line("'mkdir test/ok'");
   run_command(current_directory, "mkdir test/ok");
   if has_son_with_this_name(find_folder(current_directory, "test"), "ok") then
      put_line("mkdir test/ok ok");
   else
      put_line("mkdir test/ok is incoherent");
   end if;
   new_line;
   
   -- mkdir ./test2 in root
   put_line("'mkdir ./test2'");
   run_command(current_directory, "mkdir ./test2");
   if has_son_with_this_name(current_directory, "test2") then
      put_line("mkdir ./test2 ok");
   else
      put_line("mkdir ./test2 is incoherent");
   end if;
   new_line;
   
   -- mkdir ../test3 from test
   put_line("'mkdir ../test3' from test");
   current_directory := find_folder(current_directory, "test");
   run_command(current_directory, "mkdir ../test3");
   if has_son_with_this_name(get_root, "test3") then
      put_line("mkdir ../test3 ok");
   else
      put_line("mkdir ../test3 is incoherent");
   end if;
   new_line;
   
   
   -- mkdir /test3/ok from test
   put_line("'mkdir /test3/ok' from test");
   current_directory := get_root;
   current_directory := find_folder(current_directory, "test");
   run_command(current_directory, "mkdir /test3/ok");
   if has_son_with_this_name(find_folder(get_root, "test3"), "ok") then
      put_line("mkdir /test3/ok ok");
   else
      put_line("mkdir /test3/ok is incoherent");
   end if;
   new_line;
   new_line;
   
   -- mkdir test with test already existing
   put_line("'mkdir test' with test already existing");
   current_directory := get_root;
   run_command(current_directory, "mkdir test");
   put_line("It should raise Same_Name_Error. The couple of lines between two '===...===' should be the same :");
   Put_Line("============================================================");
   Put_Line("cannot create file or directory: A file or directory with same name already exists");
   run_command(current_directory, "mkdir test");
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
   
end test_mkdir;
