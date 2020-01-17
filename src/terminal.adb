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
      first_parameter : Unbounded_String;
      second_parameter : Unbounded_String;
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
                  first_parameter := get_substring(substrings, 3);
                  if num_element = 4 then
                     second_parameter := get_substring(substrings, 4);
                  end if;
               end if;
            else
               first_parameter := get_substring(substrings, 2);
               if num_element = 3 then
                  second_parameter := get_substring(substrings, 3);
               end if;
            end if;
         end if;
      else
         first_parameter := To_Unbounded_String("");
         second_parameter := To_Unbounded_String("");
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

            --when ls => lsCommand(option_true, To_String(first_parameter), current_dir);
            --when rm => rmCommand(option_true,To_String(first_parameter), current_dir);
            --when pwd => pwdCommand(current_dir);
            --when  cd => cdCommand(To_String(first_parameter), current_dir);
            --when mkdir => mkdirCommand(To_String(first_parameter), current_dir);
            --when  cp => cpCommand(option_true, To_String(first_parameter), current_dir);
            --when  mv => mvCommand(To_String(first_parameter), To_String(second_parameter), current_dir);
            --when  tar => tarCommand(To_String(first_parameter), current_dir);
            --when  touch => touchCommand(To_String(first_parameter), current_dir);
            --when tar => tarCommand(To_String(first_parameter, current_dir);
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
