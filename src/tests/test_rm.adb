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
   
end test_rm;
