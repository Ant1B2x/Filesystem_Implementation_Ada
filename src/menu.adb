with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with P_Constants; use P_Constants;
with P_Substrings; use P_Substrings;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure Menu is
   
   procedure print_prompt is
   begin
      put(ESC & "[31m> " & ESC & "[0m");
   end print_prompt;
   
   function get_choice (choice_min : in Integer; choice_max : in Integer) return Integer is
      choice : Integer;
   begin
      loop
         print_prompt;
         get(choice);
         skip_line; -- clear the buffer
         exit when choice in choice_min..choice_max;
      end loop;
      return choice;   
   exception
      when Data_Error =>
         put_line("Incorrect choice, try again.");
         return get_choice(choice_min, choice_max);
   end get_choice;
   
   procedure print_main_menu (current_directory : in T_Folder) is
   begin
      put_line("You are in: " & ESC & "[95m" & get_pwd(current_directory) & ESC & "[0m");
      put_line("What do you want to do?");
      put_line("1) Print name of current/working directory");
      put_line("2) Change the working directory");
      put_line("3) List directory contents");
      put_line("4) Make directory");
      put_line("5) Create an empty file");
      put_line("6) Copy file or directory");
      put_line("7) Move (rename) file or directory");
      put_line("8) Remove file or directory");
      put_line("9) Archive a directory");
      put_line("10) Clear screen");
      put_line("0) Exit");
      new_line;
   end print_main_menu;
   
   procedure cd_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      parameters : T_Substrings;
   begin
      put_line("Please specifiy a directory where you want to go, it can be a relative or an absolute path.");
      new_line;
      loop
         print_prompt;
         get_line(path, path_length);
         exit when path_length > 0;
      end loop;
      parameters := create_substrings;
      add_substring(parameters, path);
      cd_command(current_directory, create_substrings, parameters);
   end cd_menu;
   
   procedure mkdir_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      parameters : T_Substrings;
   begin
      put_line("Please specifiy a directory that you want to create, it can be a relative or an absolute path.");
      new_line;
      loop
         print_prompt;
         get_line(path, path_length);
         exit when path_length > 0;
      end loop;
      parameters := create_substrings;
      add_substring(parameters, path(1..path_length));
      mkdir_command(current_directory, create_substrings, parameters);
   end mkdir_menu;
   
   procedure ls_menu (current_directory : in T_Folder) is
      choice : Integer;
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      options : T_Substrings;
      parameters : T_Substrings;
   begin
      put_line("1) Classic");
      put_line("2) Recursive");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      if choice /= 0 then
         options := create_substrings;
         parameters := create_substrings;
         if choice = 2 then
            add_substring(options, "r");
         end if;
         put_line("From where? You can type a relative or an absolute path or leave blank for current directory.");
         new_line;
         print_prompt;
         get_line(path, path_length);
         add_substring(parameters, path(1..path_length));
         ls_command(current_directory, options, parameters);
      end if;
   end ls_menu;
   
   procedure touch_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      parameters : T_Substrings;
   begin
      put_line("Which file do you want to create? You can use absolute or relative path.");
      new_line;
      loop
         print_prompt;
         get_line(path, path_length);
         exit when path_length > 0;
      end loop;
      parameters := create_substrings;
      add_substring(parameters, path(1..path_length));
      touch_command(current_directory, create_substrings, parameters);
   end touch_menu;
   
   procedure cp_menu (current_directory : in out T_Folder) is
      choice : Integer;
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      options : T_Substrings;
      parameters : T_Substrings;
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      if choice /= 0 then
         options := create_substrings;
         parameters := create_substrings;
         if choice = 2 then
            add_substring(options, "r");
         end if;
         
         put_line("From where? You can type a relative or an absolute path.");
         new_line;
         loop
            print_prompt;
            get_line(path, path_length);
            exit when path_length > 0;
         end loop;
         add_substring(parameters, path(1..path_length));
         
         put_line("To where? You can type a relative or an absolute path.");
         new_line;
         print_prompt;
         get_line(path, path_length);
         add_substring(parameters, path(1..path_length));
         
         cp_command(current_directory, options, parameters);
      end if;
   end cp_menu;
   
   procedure mv_menu (current_directory : in out T_Folder) is
      choice : Integer;
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      options : T_Substrings;
      parameters : T_Substrings;
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      if choice /= 0 then
         options := create_substrings;
         parameters := create_substrings;
         if choice = 2 then
            add_substring(options, "r");
         end if;
         
         put_line("From where? You can type a relative or an absolute path.");
         new_line;
         loop
            print_prompt;
            get_line(path, path_length);
            exit when path_length > 0;
         end loop;
         add_substring(parameters, path(1..path_length));
         
         put_line("To where? You can type a relative or an absolute path.");
         new_line;
         loop
            print_prompt;
            get_line(path, path_length);
            exit when path_length > 0;
         end loop;
         add_substring(parameters, path(1..path_length));
         
         mv_command(current_directory, options, parameters);
      end if;
      
   end mv_menu;
   
   procedure rm_menu (current_directory : in out T_Folder) is
      choice : Integer;
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      options : T_Substrings;
      parameters : T_Substrings;
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      if choice /= 0 then
         options := create_substrings;
         parameters := create_substrings;
         if choice = 2 then
            add_substring(options, "r");
         end if;
         
         put_line("What do you want to delete? You can type a relative or an absolute path.");
         new_line;
         loop
            print_prompt;
            get_line(path, path_length);
            exit when path_length > 0;
         end loop;
         add_substring(parameters, path(1..path_length));
         
         rm_command(current_directory, options, parameters);
      end if;
   end rm_menu;
   
   procedure tar_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING);
      path_length : Integer;
      
      parameters : T_Substrings;
   begin
      put_line("Please specifiy a directory that you want to archive, it can be a relative or an absolute path or blank for current directory.");
      new_line;
      print_prompt;
      get_line(path, path_length);
      parameters := create_substrings;
      add_substring(parameters, path);
      tar_command(current_directory, create_substrings, parameters);
   end tar_menu;
   
   choice : Integer;
   current_directory : T_Folder;
begin
   current_directory := get_root;
   loop
      print_main_menu(current_directory);
      choice := get_choice(0, 10);
      case (choice) is
         when 1 =>
            pwd_command(current_directory, create_substrings, create_substrings);
         when 2 =>
            cd_menu(current_directory);
         when 3 =>
            ls_menu(current_directory);
         when 4 =>
            mkdir_menu(current_directory);
         when 5 =>
            touch_menu(current_directory);
         when 6 =>
            cp_menu(current_directory);
         when 7 =>
            mv_menu(current_directory);
         when 8 =>
            rm_menu(current_directory);
         when 9 =>
            tar_menu(current_directory);
         when 10 =>
            clear_command;
         when others =>
            null;
      end case;
      exit when choice = 0;
   end loop;

end Menu;