with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure Terminal is
   current_directory : T_Folder; -- the current directory, root by default
   command_line : String(1..LMAX_STRING); -- the command line entered by the user
   command_line_length : Integer; -- the length of the command line entered by the user
begin
   -- begin in the root directory
   current_directory := get_root;
   -- while the user do not type "exit", loop
   loop
      -- print colored prompt with pwd
      put(ASCII.ESC & "[31m" & get_pwd(current_directory) & " > " & ASCII.ESC & "[0m");
      -- get the command line from the user
      get_line(command_line, command_line_length);
      -- if the command line is "exit", exit the loop
      exit when command_line(1..command_line_length) = "exit";
      -- if the command line isn't blank, run the command
      if command_line_length > 0 then
         run_command(current_directory, command_line(1..command_line_length));
      end if;
   end loop;

end Terminal;
