with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Folder; use P_Folder;
--with P_Commands; use P_Commands;

procedure Terminal is

   type encoded_commands is (ls,rm,pwd,cd,mkdir,cp,mv,touch,tar,help);

   procedure run_command(command: String; current_dir: T_Folder) is
      substrings : T_Substrings;
      option_true : Boolean := False;
      arguments : T_Substrings;
      num_element : Integer;
   begin
      substrings := split_string(command, " ");

      num_element := P_Substrings.get_nb_substrings(substrings);
      if num_element > 1 then
         -- help command case
         if encoded_commands'Value(get_substring_to_string(substrings, 1)) = help
           and get_substring_to_string(substrings, 2) /= "" then
            option_true := True;
         else
            if get_substring_to_string(substrings, 2) = "-r" then
               option_true := True;
               if num_element > 2 then
                  arguments := get_substrings(substrings, 3, get_nb_substrings(substrings));
               end if;
            else
               arguments := get_substrings(substrings, 2, get_nb_substrings(substrings));
            end if;
         end if;
      else
         arguments := create_substrings;
      end if;

      begin
         case encoded_commands'Value(get_substring_to_string(substrings, 1)) is
            when ls => put_line("aile hesse");
            when rm => put_line("air aime");
            when pwd => put_line("estewban");
            when cd => put_line("sait dés");
            when mkdir => put_line("aime qu'à dire");
            when cp => put_line("sait pet");
            when mv => put_line("aime vé");
            when touch => put_line("toutche");
            when tar => put_line("tard");
            when help => put_line("aile peu");

            --when ls => lsCommand(option_true, arguments, current_dir);
            --when rm => rmCommand(option_true,arguments, current_dir);
            --when pwd => pwdCommand(current_dir);
            --when  cd => cdCommand(arguments, current_dir);
            --when mkdir => mkdirCommand(arguments, current_dir);
            --when  cp => cpCommand(option_true, arguments, current_dir);
            --when  mv => mvCommand(arguments, current_dir);
            --when  tar => tarCommand(arguments, current_dir);
            --when  touch => touchCommand(arguments, current_dir);
            --when tar => tarCommand(arguments, current_dir);
            --when help => help_command(option_true, get_substring_to_string(substrings, 2));
         end case;
      exception
         when Constraint_Error =>
            put_line("Unknown command.");
            put_line("Try 'help' to see a list of available commands.");
      end;

   end run_command;

   command : String(1..LMAX_STRING);
   command_length : Integer;
begin

   loop
      put("> ");
      get_line(command, command_length);
      exit when command(1..command_length) = "exit";

      run_command(command(1..command_length), P_Folder.get_root);
   end loop;


end Terminal;
