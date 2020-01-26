with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure Terminal is
   current_directory : T_Folder;
   command : String(1..LMAX_STRING);
   command_length : Integer;
begin

   current_directory := get_root;
   loop
      put(ASCII.ESC & "[31m" & get_pwd(current_directory) & " > " & ASCII.ESC & "[0m");
      get_line(command, command_length);
      exit when command(1..command_length) = "exit";

      if command_length > 0 then
         run_command(current_directory, command(1..command_length));
      end if;
   end loop;

end Terminal;
