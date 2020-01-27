with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_File; use P_File;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure test_cd is
   current_directory : T_Folder;
begin
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
   
   -- cd 
   
   
end test_cd;
