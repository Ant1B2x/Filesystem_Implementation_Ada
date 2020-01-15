with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;

procedure Terminal is

   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch);
   procedure run_command(current_dir: T_Folder; command: String) is
      substrings : T_SubStrings;
      option_true : Boolean := False;
      first_parameter : Unbounded_String;
      second_parameter : Unbounded_String;
      num_element : Integer;
   begin
      substrings := split_string(command, ' ');

      num_element := P_Substrings.get_nb_values(substrings);
      if num_element > 1 then
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
      else
         first_parameter := To_Unbounded_String("");
         second_parameter := To_Unbounded_String("");
      end if;
      case encoded_commands'Value(get_substring_to_string(substrings, 1)) is
         when ls => lsCommand(option_true, To_String(first_parameter), current_dir);
         when rm => rmCommand(option_true,To_String(first_parameter), current_dir);
         when pwd => pwdCommand(current_dir);
         when  cd => cdCommand(To_String(first_parameter), current_dir);
         when mkdir => mkdirCommand(To_String(first_parameter), current_dir);
         when  cp => cpCommand(option_true, To_String(first_parameter), current_dir);
         when  mv => mvCommand(To_String(first_parameter), To_String(second_parameter), current_dir);
         when  tar => tarCommand(To_String(first_parameter), current_dir);
         when  touch => touchCommand(To_String(first_parameter), current_dir);
         when others => Put_Line("> Unknown command.");
      end case;
   end run_command;

   command : String(1..LMAX_STRING);
begin

   -- code
   null;
end Terminal;
