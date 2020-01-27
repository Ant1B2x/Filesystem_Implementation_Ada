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
      put_line("is_root(current_directory)");
   else
      put_line("is_root(current_directory) is incoherent");
   end if;
   new_line;
   
   -- cd . in root
   put_line("cd . in root");
   current_directory := get_root;
   run_command(current_directory, "cd .");
   if is_root(current_directory) then
      put_line("is_root(current_directory)");
   else
      put_line("is_root(current_directory) is incoherent");
   end if;
   new_line;
   
   -- cd test
   put_line("cd test in root");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   if get_name(current_directory) = "test" then
      put_line("get_name(current_directory) = 'test'");
   else
      put_line("get_name(current_directory) = 'test' is incoherent");
   end if;
   new_line;
   
   -- cd .. from test
   put_line("cd .. from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ..");
   if is_root(current_directory) then
      put_line("is_root(current_directory)");
   else
      put_line("is_root(current_directory) is incoherent");
   end if;
   new_line;
   
   -- cd ../test2 from test
   put_line("cd ../test2 from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ../test2");
   if get_name(current_directory) = "test2" then
      put_line("get_name(current_directory) = 'test2'");
   else
      put_line("get_name(current_directory) = 'test2' is incoherent");
   end if;
   new_line;
   
   -- cd ./ from test
   put_line("cd ./ from test");
   current_directory := get_root;
   run_command(current_directory, "cd test");
   run_command(current_directory, "cd ./");
   if get_name(current_directory) = "test" then
      put_line("get_name(current_directory) = 'test'");
   else
      put_line("get_name(current_directory) = 'test' is incoherent");
   end if;
   new_line;
   
   -- cd ./.././test/./ok from test2
   put_line("cd ./.././test/./ok from test2");
   current_directory := get_root;
   run_command(current_directory, "cd test2");
   run_command(current_directory, "cd ./.././test/./ok");
   if get_name(current_directory) = "ok" then
      put_line("get_name(current_directory) = 'ok'");
   else
      put_line("get_name(current_directory) = 'ok' is incoherent");
   end if;
   new_line;
   
   
end test_cd;
