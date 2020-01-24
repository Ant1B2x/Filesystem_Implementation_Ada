with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure Terminal is

   procedure run_command(command: in String; current_dir: in out T_Folder) is
      substrings : T_Substrings;
      option_true : Boolean := False;
      arguments : T_Substrings;
      num_element : Integer;
   begin
      substrings := split_string(command, ' ');

      num_element := P_Substrings.get_nb_substrings(substrings);
      if num_element > 1 then
         -- help command case

         if get_substring_to_string(substrings, 2) = "-r" then
            option_true := True;
            if num_element > 2 then
               arguments := get_substrings(substrings, 3, get_nb_substrings(substrings));
            end if;
         else
            arguments := get_substrings(substrings, 2, get_nb_substrings(substrings));
         end if;

      else
         arguments := create_substrings;
      end if;

      begin
         case encoded_commands'Value(get_substring_to_string(substrings, 1)) is
            when ls => ls_command(current_dir, arguments, option_true);
            when rm => rm_command(current_dir, arguments, option_true);
            when pwd => pwd_command(current_dir);
            when cd => cd_command(current_dir, arguments);
            when mkdir => mkdir_command(current_dir, arguments);
            when cp => cp_command(current_dir, arguments, option_true);
            when mv => mv_command(current_dir, arguments, option_true);
            when touch => touch_command(current_dir, arguments);
            when tar => tar_command(current_dir, arguments);
            when help => help_command(arguments);
            when clear => clear_command;
         end case;
      exception
         when Constraint_Error =>
            put_line("command not found");
            put_line("Try 'help' to see a list of available commands.");
         when Wrong_Arguments_Number_Error =>
            put_line("missing operand");
            put_line("Try help '" & get_substring_to_string(substrings, 1) & "' for more information.");
         when Invalid_Folder_Error =>
            Put_Line("The specified path countain an unexisting folder. Can't go through it.");
      end;

   end run_command;

   folder : T_Folder;
   command : String(1..LMAX_STRING);
   command_length : Integer;
begin

   folder := get_root;
   loop
      put(ESC & "[31m" & get_pwd(folder) & " > " & ESC & "[0m");
      get_line(command, command_length);
      exit when command(1..command_length) = "exit";

      if command_length > 0 then
         run_command(command(1..command_length), folder);
      end if;
   end loop;


end Terminal;
