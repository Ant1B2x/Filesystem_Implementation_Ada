package body P_Parser is
   
   function create_substrings return T_Substrings is
   begin
      return P_Substrings.create;
   end create_substrings;
   
   procedure add_substring (substrings : in out T_Substrings; substring : in Unbounded_String) is
   begin
      P_Substrings.add_value(substrings, substring);
   end add_substring;
   
   function split_string (original : in String; separator : in Character) return T_Substrings is
      substrings : T_Substrings;
      i: Integer; -- index
      substring_first : Integer;
   begin
      substrings := P_Substrings.create;
      i := 1;
      substring_first := 1;
      while i <= original'Length loop
         if original(i) = separator then
            add_substring(substrings, To_Unbounded_String(original(substring_first..(i - 1))));
            while i < original'Length and then original(i + 1) = separator loop
               i := i + 1;
            end loop;
            substring_first := i + 1;
         end if;
         i := i + 1;
      end loop;
      if original(original'Last) /= separator then
         add_substring(substrings, To_Unbounded_String(original(substring_first..original'Last)));
      end if;
      return substrings;
   end split_string;
   
   function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String is
   begin
      return P_Substrings.get_value(substrings, index);
   end get_substring;
   
   function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String is
   begin
      return To_String(get_substring(substrings, index));
   end get_substring_to_string;

   
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
   
end P_Parser;
