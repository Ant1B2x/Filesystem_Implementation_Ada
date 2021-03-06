with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with P_Constants; use P_Constants;
with P_Folder; use P_Folder;
with P_Commands; use P_Commands;

procedure Menu is
   
   -- Role : Print a prompt (">")
   -- Parameters : /
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure print_prompt is
   begin
      -- print prompt in red
      put(ASCII.ESC & "[31m> " & ASCII.ESC & "[0m");
   end print_prompt;
   
   -- Role : Get an user choice as Integer between a given range and return it
   -- Parameters :
   --    choice_min (in Integer) : Minimum of the range
   --    choice_max (in Integer) : Maximum of the range
   -- Return :
   --    Integer : The choice of the user
   -- Preconditions : /
   -- Postconditions : /
   function get_choice (choice_min : in Integer; choice_max : in Integer) return Integer is
      choice : Integer; -- choice typed by the user
   begin
      -- while choice is not between choice_min and choice_max
      loop
         -- show de prompt
         print_prompt;
         -- get the choice
         begin
            get(choice);
            skip_line; -- clear the buffer
         exception
            -- when data error, like, for example, entering Character
            when Data_Error =>
               skip_line; -- clear the buffer
         end;
         exit when choice in choice_min..choice_max;
         -- print error message if the choice is not between choice_min and choice_max
         put_line("Incorrect choice, try again.");
      end loop;
      return choice;
   end get_choice;
   
   -- Role : Print the main menu
   -- Parameters :
   --    current_directory (in T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure print_main_menu (current_directory : in T_Folder) is
   begin
      -- print the menu with the differents commands
      put_line("You are in: " & ASCII.ESC & "[31m" & get_pwd(current_directory) & ASCII.ESC & "[0m");
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
   
   -- Role : Print the menu of the "cd" command, get the parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure cd_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of path entered by the user
   begin
      put_line("Please specifiy a directory where you want to go, it can be a relative or an absolute path.");
      new_line;
      get_line(path, path_length);
      -- run the command "cd [path]"
      run_command(current_directory, command_to_string(cd) & " " & path(1..path_length));
   end cd_menu;
   
   -- Role : Print the menu of the "ls" command, get the options and parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure ls_menu (current_directory : in out T_Folder) is
      choice : Integer; -- choice entered by the user
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of path entered by the user
   begin
      put_line("1) Classic");
      put_line("2) Recursive");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      -- if the user did not go back
      if choice /= 0 then
         put_line("From where? You can type a relative or an absolute path or leave blank for current directory.");
         new_line;
         print_prompt;
         -- get the path
         get_line(path, path_length);
         -- if this is a recursive ls, run the command "ls -r [path]"
         if choice = 2 then
            run_command(current_directory, command_to_string(ls) & " -r " & path(1..path_length));
         -- else, run "ls [path]"
         else
            run_command(current_directory, command_to_string(ls) & " " & path(1..path_length));
         end if;
      end if;
   end ls_menu;
   
   -- Role : Print the menu of the "mkdir" command, get the parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure mkdir_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of the path entered by the user
   begin
      put_line("Please specifiy a directory that you want to create, it can be a relative or an absolute path.");
      new_line;
      -- while the path is blank, get the path
      loop
         print_prompt;
         get_line(path, path_length);
         exit when path_length > 0;
      end loop;
      -- run "mkdir [path]"
      run_command(current_directory, command_to_string(mkdir) & " " & path(1..path_length));
   end mkdir_menu;
   
   -- Role : Print the menu of the "touch" command, get the parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure touch_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of the path entered by the user
   begin
      put_line("Which file do you want to create? You can use absolute or relative path.");
      new_line;
      -- while the path is blank, get the path
      loop
         print_prompt;
         get_line(path, path_length);
         exit when path_length > 0;
      end loop;
      -- run "touch [path]"
      run_command(current_directory, command_to_string(touch) & " " & path(1..path_length));
   end touch_menu;
   
   -- Role : Print the menu of the "cp" command, get the options and parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure cp_menu (current_directory : in out T_Folder) is
      choice : Integer; -- choice entered by the user
      source_path : String(1..LMAX_STRING); -- source path entered by the user
      source_path_length : Integer; -- length of the source path entered by the user
      destination_path : String(1..LMAX_STRING); -- destination path entered by the user
      destination_path_length : Integer; -- length of the destination path entered by the user
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      -- if the user did not go back
      if choice /= 0 then
         put_line("From where? You can type a relative or an absolute path.");
         new_line;
         -- while the source path is blank, get the source path
         loop
            print_prompt;
            get_line(source_path, source_path_length);
            exit when source_path_length > 0;
         end loop;
         put_line("To where? You can type a relative or an absolute path.");
         new_line;
         -- while the destination path is blank, get the destination path
         loop
            print_prompt;
            get_line(destination_path, destination_path_length);
            exit when destination_path_length > 0;
         end loop;
         -- if this is a recurive cp, run "cp -r [source_path] [destination_path]"
         if choice = 2 then
            run_command(current_directory, command_to_string(cp) & " -r " & source_path(1..source_path_length) & " " & destination_path(1..destination_path_length));
         -- else, run "cp [source_path] [destination_path]"
         else
            run_command(current_directory, command_to_string(cp) & " " & source_path(1..source_path_length) & " " & destination_path(1..destination_path_length));
         end if;
      end if;
   end cp_menu;
   
   -- Role : Print the menu of the "mv" command, get the options and parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure mv_menu (current_directory : in out T_Folder) is
      choice : Integer; -- choice entered by the user
      source_path : String(1..LMAX_STRING); -- source path entered by the user
      source_path_length : Integer; -- length of the source path entered by the user
      destination_path : String(1..LMAX_STRING); -- destination path entered by the user
      destination_path_length : Integer; -- length of the destination path entered by the user
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      -- if the user did not go back
      if choice /= 0 then
         put_line("From where? You can type a relative or an absolute path.");
         new_line;
         -- while the source path is blank, get the source path
         loop
            print_prompt;
            get_line(source_path, source_path_length);
            exit when source_path_length > 0;
         end loop;
         put_line("To where? You can type a relative or an absolute path.");
         new_line;
         -- while the destination path is blank, get the destination path
         loop
            print_prompt;
            get_line(destination_path, destination_path_length);
            exit when destination_path_length > 0;
         end loop;
         -- if this is a recursive move, run "mv -r [source_path] [destination_path]"
         if choice = 2 then
            run_command(current_directory, command_to_string(mv) & " -r " & source_path(1..source_path_length) & " " & destination_path(1..destination_path_length));
         -- else, run "mv [source_path] [destination_path]"
         else
            run_command(current_directory, command_to_string(mv) & " " & source_path(1..source_path_length) & " " & destination_path(1..destination_path_length));
         end if;
      end if;
   end mv_menu;
   
   -- Role : Print the menu of the "rm" command, get the options and parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure rm_menu (current_directory : in out T_Folder) is
      choice : Integer; -- choice entered by the user
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of the path entered by the user
   begin
      put_line("1) File");
      put_line("2) Directory");
      put_line("0) Back");
      new_line;
      choice := get_choice(0, 2);
      -- if the user did not go back
      if choice /= 0 then
         put_line("What do you want to delete? You can type a relative or an absolute path.");
         new_line;
         -- while the path is blank, get the path
         loop
            print_prompt;
            get_line(path, path_length);
            exit when path_length > 0;
         end loop;
         -- if this is a recursive rm, run "rm -r [path]"
         if choice = 2 then
            run_command(current_directory, command_to_string(rm) & " -r " & path(1..path_length));
         -- else, run "rm [path]"
         else
            run_command(current_directory, command_to_string(rm) & " " & path(1..path_length));
         end if;
      end if;
   end rm_menu;
   
   -- Role : Print the menu of the "tar" command, get the parameters from the user, and run the command
   -- Parameters :
   --    current_directory (in out T_Folder) : Directory where the user is currently located in
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure tar_menu (current_directory : in out T_Folder) is
      path : String(1..LMAX_STRING); -- path entered by the user
      path_length : Integer; -- length of the path entered by the user
   begin
      put_line("Please specifiy a directory that you want to archive, it can be a relative or an absolute path or blank for current directory.");
      new_line;
      print_prompt;
      -- get the path
      get_line(path, path_length);
      -- run "tar [path]"
      run_command(current_directory, command_to_string(tar) & " " & path(1..path_length));
   end tar_menu;
   
   choice : Integer; -- choice entered by the user
   current_directory : T_Folder; -- directory we are currently in
begin
   -- current_directory is equal to root by default
   current_directory := get_root;
   -- while the user do not type "0", get his choices and execute the corresponding routines
   loop
      print_main_menu(current_directory);
      choice := get_choice(0, 10);
      case (choice) is
         when 1 =>
            run_command(current_directory, command_to_string(pwd));
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
            run_command(current_directory, command_to_string(clear));
         when others =>
            null;
      end case;
      exit when choice = 0;
   end loop;

end Menu;
