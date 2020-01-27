package body P_Commands is
   
   -- #################################################### PUBLIC SUBROUTINES ############################################################
   
   function command_to_string (encoded_command : in E_Encoded_Commands) return String is
   begin
      return E_Encoded_Commands'Image(encoded_command);
   end command_to_string;
   
   procedure run_command(current_directory : in out T_Folder; command_line : in String) is
      splitted_line : T_Substrings; -- splitted command line
      nb_arguments : Integer; -- number of arguments in the command line (counting the called command)
      
      options : T_Substrings := create_substrings; -- options to pass to the called command
      parameters : T_Substrings := create_substrings; -- parameters to pass to the called command
   begin
      -- split the command line
      splitted_line := split_string(command_line, ' ');
      nb_arguments := get_nb_substrings(splitted_line);

      -- if we have more than one argument, that means we have options or parameters
      if nb_arguments > 1 then
         -- decode arguments from 2 to N in the options and parameters variables
         get_options_and_parameters(get_substrings(splitted_line, 2, nb_arguments), options, parameters);
      end if;

      -- execute the called command
      case E_Encoded_Commands'Value(get_substring_to_string(splitted_line, 1)) is
         when ls => ls_command(current_directory, options, parameters);
         when rm => rm_command(current_directory, options, parameters);
         when pwd => pwd_command(current_directory, options, parameters);
         when cd => cd_command(current_directory, options, parameters);
         when mkdir => mkdir_command(current_directory, options, parameters);
         when cp => cp_command(current_directory, options, parameters);
         when mv => mv_command(current_directory, options, parameters);
         when touch => touch_command(current_directory, options, parameters);
         when tar => tar_command(current_directory, options, parameters);
         when help => help_command(options, parameters);
         when clear => clear_command;
      end case;
   exception
      when Invalid_Character_Error =>
         put_line("A path you entered contain an invalid character.");
      when Invalid_Folder_Error =>
         put_line("A specified path is incorrect, no such directory.");
      when Invalid_File_Error =>
         put_line("A specified path is incorrect, no such file.");
      when Copy_Into_Itself_Error =>
         put_line("Cannot copy a directory into itself.");
      When Same_Name_Error =>
         put_line("Cannot create file or directory: A file or directory with same name already exists.");
      when Empty_Option_Error =>
         put_line("Empty option.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      when Not_Handled_Option_Error =>
         put_line("Not handled option.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      when Wrong_Parameters_Number_Error =>
         put_line("Wrong number of parameters.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      --when Constraint_Error =>
      --   put_line("command not found");
      --   put_line("Try 'help' to see a list of available commands.");
   end run_command;
   
   -- ############################################ VARIOUS PRIVATE SUBROUTINES ###########################################################
   
   procedure get_options_and_parameters (arguments : in T_Substrings; options : in out T_Substrings; parameters : in out T_Substrings) is
   begin
      -- For every argument
      for i in 1..get_nb_substrings(arguments) loop
         -- If it starts with a "-", then it's an option
         if get_substring_to_string(arguments, i)(1) = '-' then
            -- If we only have "-", we raise an error
            if get_substring_to_string(arguments, i)'Length < 2 then
               raise Empty_Option_Error;
            end if;
            -- We start at 2 because we need to get all options, and not the "-" character
            for j in 2..get_substring_to_string(arguments, i)'Length loop
               -- We have to transform it into a string, because the (j) part return a Character, and we add it to the options array
               add_substring(options, get_substring_to_string(arguments, i)(j) & "");
            end loop;
         else
            -- Else it's an argument and we add it to the arguments array
            add_substring(parameters, get_substring_to_string(arguments, i));
         end if;
      end loop;
   end get_options_and_parameters;
   
   function split_options (options_as_string : in String) return T_Substrings is
      splitted_option : T_Substrings; -- splitted options following "-" separator (like ["-ab"], ["-cd"])
      options : T_Substrings; -- all options (like ["a","b","c","d"])
   begin
      options := create_substrings;
      splitted_option := split_string(options_as_string, '-');
      for i in 1.. get_nb_substrings(splitted_option) loop
         -- If there is multiple option wrote after a "-" (like "-rf")
         if get_substring_to_string(splitted_option, i)'Length > 1 then
            -- We add each one of them to the options
            for j in 1.. get_substring_to_string(splitted_option, i)'Length loop
               add_substring(options, ""&get_substring_to_string(splitted_option, i)(j));
            end loop;
         -- Else, It's a simple option like "-r"
         else
            -- we add the option to the options
            add_substring(options, ""&get_substring_to_string(splitted_option, i)(1));
         end if;
      end loop;
      return options;
   end split_options;
   
   function contains_option (options : in T_Substrings; option : in Character) return Boolean is
   begin
      for i in 1..get_nb_substrings(options) loop
         -- return True when the desired option is found
         if get_substring_to_string(options, i)(1) = option then
            return True;
         end if;
      end loop;
      -- return False anyway
      return False;
   end contains_option;
   
   function only_handled_options (options : in T_Substrings; handled_options : in String) return Boolean is
      splitted_handled_options : T_Substrings;
   begin
      splitted_handled_options := split_options(handled_options);
      for i in 1..get_nb_substrings(options) loop
         -- return False if a provided option is not supported
         if not contains_option(splitted_handled_options, get_substring_to_string(options, i)(1)) then
            return False;
         end if;
      end loop;
      -- return True anyway
      return True;
   end only_handled_options;
   
   function go_to_folder (original_directory : in T_Folder; path: in String; stop_at_penultimate: in Boolean := False) return T_Folder is
      current: T_Folder; -- current directory
      path_tree: T_Substrings; -- directory tree
      penultimate: Integer; -- 1 if stop_at_penultimate, 0 else
   begin
      -- we can't go to a blank path
      if path'Length <= 0 then
         raise Invalid_Folder_Error;
      end if;
      
      -- if this is an absolute path
      if path(path'First) = FILE_SEPARATOR then
         -- start from root
         current := get_root;
      else
         -- else, start from the current folder
         current := original_directory;            
      end if;
      
      -- we might want to stop at the penultimate of the given path, when using mkdir for example
      penultimate := (if stop_at_penultimate then 1 else 0);
      -- follow the path from the defined start
      path_tree := split_string(path, FILE_SEPARATOR);
      for i in 1..get_nb_substrings(path_tree) - penultimate loop
         -- if the following directory doesn't exist, the path is invalid, raise an exception
         if is_null(find_folder(current, get_substring_to_string(path_tree, i))) then
            raise Invalid_Folder_Error;
         end if;
         -- take the next sibling as current
         current := find_folder(current, get_substring_to_string(path_tree, i));
      end loop;
      return current;
   end go_to_folder;
   
   -- Here we choose to use Unbounded_String because we don't know the length of the return
   function get_name_from_path (path : in Unbounded_String) return Unbounded_String is
      nb_substrings : Integer; -- represents how many folders are separated by "/" in the path
   begin
      nb_substrings := get_nb_substrings(split_string(To_String(path), FILE_SEPARATOR));
      -- if only "/" is provided in path, return "/"
      if nb_substrings = 0 then
         return To_Unbounded_String(""&FILE_SEPARATOR);
      end if;
      return get_substring(split_string(To_String(path), FILE_SEPARATOR), nb_substrings);
   end get_name_from_path;
   
   function calculate_size (current_directory : in T_Folder) return Integer is
      current_folder_size: Integer; -- folder size, incremented at each file or directory
   begin
      -- folder size is 10Ko by default
      current_folder_size := FOLDER_SIZE;
      -- + files size
      for i in 1..get_nb_files(current_directory) loop
         current_folder_size := current_folder_size + get_size(get_file(current_directory, i));
      end loop;
      -- + subfolders size
      for i in 1..get_nb_folders(current_directory) loop
         current_folder_size := current_folder_size + calculate_size(get_folder(current_directory, i));
      end loop;
      return current_folder_size;
   end calculate_size;
   
   function compare_T_R_Siblings (sibling1, sibling2 : in T_R_Sibling) return Boolean is
   begin
      -- return true if the name of the first sibling is inferior to the name of the second
      if sibling1.name <= sibling2.name then
         return True;
      -- else, return false
      else
         return False;
      end if;
   end compare_T_R_Siblings;
   
   function create_siblings_set (directory : in T_Folder) return T_Siblings_Set is
      siblings_set : T_Siblings_Set; -- returned sibling set
      new_element : T_R_Sibling; -- new element of the set
   begin
      -- add each folder
      for i in 1.. get_nb_folders(directory) loop
         new_element.name := To_Unbounded_String(get_name(get_folder(directory, i)));
         new_element.is_folder := True;
         siblings_set.insert(new_element);
      end loop;
      -- add each file
      for i in 1.. get_nb_files(directory) loop
         new_element.name := To_Unbounded_String(get_name(get_file(directory, i)));
         new_element.is_folder := False;
         siblings_set.insert(new_element);
      end loop;
      return siblings_set;
   end create_siblings_set;
   
   procedure display_folders_and_files (siblings_set : in T_Siblings_Set) is
   begin
      for sibling of siblings_set loop
         -- if this is a folder, color it
         if sibling.is_folder then
            put(ASCII.ESC & "[95m");
            put(To_String(sibling.name) & "  ");
            put(ASCII.ESC & "[0m");
         else
            put(To_String(sibling.name) & "  ");
         end if;
      end loop;
      new_line;
   end display_folders_and_files;
   
   procedure folder_deep_copy (folder1 : in T_Folder; folder2 : in out T_Folder) is
      original_file : T_File; -- original file
      new_file : T_File; -- copy of original file
      original_folder : T_Folder; -- original folder
      new_folder : T_Folder; -- copy of a folder
   begin
      -- we can't copy a folder from itself to itself
      if get_pwd(folder1) = get_pwd(folder2) or (not is_root(folder2) and then get_pwd(folder1) = get_pwd(get_parent(folder2))) then
         raise Copy_Into_Itself_Error;
      end if;
      -- copy all files from original folder
      for i in 1..get_nb_files(folder1) loop
         original_file := get_file(folder1, i);
         new_file := clone(original_file, get_pwd(folder2));
         add_file(folder2, new_file);
      end loop;
      -- copy all folders from original folder
      for i in 1..get_nb_folders(folder1) loop
         original_folder := get_folder(folder1, i);
         new_folder := create(get_name(original_folder), folder2, get_rights(original_folder));
         folder_deep_copy(get_folder(folder1, i), new_folder);
      end loop;
   end folder_deep_copy;
   
   procedure print_global_help is
   begin
      put_line("Available commands:");
      put_line("pwd      print name of current/working directory");
      put_line("cd       change the working directory");
      put_line("ls       list directory contents");
      put_line("mkdir    make directories");
      put_line("touch    create empty file");
      put_line("cp       copy files and directories");
      put_line("mv       move (rename) files");
      put_line("rm       remove files or directories");
      put_line("tar      archive a directory");
      put_line("clear    clear the terminal screen");
      put_line("help     show this menu");
      put_line("exit     cause the shell to exit");
      new_line;
      put_line("You can get more help by specifying a command.");
      put_line("For example, try 'help ls'.");
   end print_global_help;

   procedure print_specific_help (command : in String) is
   begin
      begin
         case E_Encoded_Commands'Value(command) is
            when pwd =>
               put_line("NAME");
               put_line("  pwd - print name of current/working directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  pwd");
            when cd =>
               put_line("NAME");
               put_line("  cd - change the working directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  cd DIRECTORY");
            when ls =>
               put_line("NAME");
               put_line("  ls - list directory contents");
               new_line;
               put_line("SYNOPSIS");
               put_line("  ls [-r] DIRECTORY");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      list recursively");
            when mkdir =>
               put_line("NAME");
               put_line("  mkdir - make directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  mkdir DIRECTORY");
            when touch =>
               put_line("NAME");
               put_line("  touch - create empty file");
               new_line;
               put_line("SYNOPSIS");
               put_line("  touch FILE");
            when cp =>
               put_line("NAME");
               put_line("  cp - copy files and directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  cp [-r] SOURCE DEST");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      copy a directory, copy file if omitted");
            when mv =>
               put_line("NAME");
               put_line("  mv - move (rename) files");
               new_line;
               put_line("SYNOPSIS");
               put_line("  mv [-r] SOURCE DEST");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      move a directory, move file if omitted");
            when rm =>
               put_line("NAME");
               put_line("  rm - remove files or directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  rm [-r] FILE");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      remove a directory, remove file if omitted");
            when tar =>
               put_line("NAME");
               put_line("  tar - archive a directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  tar FILE");
            when clear =>
               put_line("NAME");
               put_line("  clear - clear the terminal screen");
               new_line;
               put_line("SYNOPSIS");
               put_line("  clear");
            when help =>
               print_global_help;
         end case;
      exception
         when Constraint_Error =>
            put_line("No help entry for this command.");
            new_line;
            print_global_help;
      end;
   end print_specific_help;
   
   -- ################################################ PRIVATE COMMANDS ##################################################################
   
   procedure pwd_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 0 then
         raise Wrong_Parameters_Number_Error;
      end if;
      put_line(get_pwd(current_directory));
   end pwd_command;
   
   procedure cd_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a path is specified, cd to this path
      if get_substring_to_string(parameters, 1)'Length > 0 then
         current_directory := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      else
      -- if no path is specified, cd to root directory
         current_directory := get_root;
      end if;
   end cd_command;
   
   procedure ls_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      current : T_Folder; -- represents the folder where we are doing the "ls" in
   begin
      -- ls only supports "-r" option
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a path is provided, do ls in this path
      if get_substring_to_string(parameters, 1)'Length > 0 then
         current := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      -- else, ls in the current directory
      else
         current := current_directory;
      end if;
      -- if options contain r, do a recursive ls on each subfolder
      if contains_option(options, 'r') then
         put_line(".:");
         display_folders_and_files(create_siblings_set(current));
         for i in 1.. get_nb_folders(current) loop
            ls_r_command(get_folder(current, i), To_Unbounded_String("."));
         end loop;
      -- else, do a standard ls
      else
         display_folders_and_files(create_siblings_set(current));
      end if;
   end ls_command;
   
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String) is
      current_path : Unbounded_String; -- represents the current relative path from the folder we are doing the "ls -r" in
   begin
      -- print current path and ":"
      current_path := preceding_path & FILE_SEPARATOR & get_name(current_directory);
      new_line;
      put_line(To_String(current_path) & ":");
      -- print current directory content
      display_folders_and_files(create_siblings_set(current_directory));
      for i in 1.. get_nb_folders(current_directory) loop
         ls_r_command(get_folder(current_directory,i), current_path);
      end loop;
   end ls_r_command;
   
   procedure mkdir_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      new_directory : T_Folder; -- the directory we are creating
      new_directory_name : Unbounded_String; -- the name of the directory we are creating
      parent : T_Folder; -- the parent of the new directory
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- go to the parent of the new directory
      parent := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      -- create the new directory with the given name
      new_directory_name := get_name_from_path(get_substring(parameters, 1));
      new_directory := create(To_String(new_directory_name), parent);
   end mkdir_command;
   
   procedure touch_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings)is
      new_file : T_File; -- the file we are creating
      new_file_name: Unbounded_String; -- the name of the file we are creating
      parent : T_Folder; -- the parent of the new file
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- go to the parent of the new file
      parent := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      -- create the new file with the given name
      new_file_name := get_name_from_path(get_substring(parameters, 1));
      new_file := create(To_String(new_file_name), get_pwd(parent));
      add_file(parent, new_file);
   end touch_command;
   
   procedure cp_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      new_name : Unbounded_String; -- the name of the new file / folder
      destination_folder_parent : T_Folder; -- the parent of the destination file / folder
      source_folder : T_Folder; -- the source folder from where we are copying / we copy
      destination_folder : T_Folder; -- the destination folder in which we copy
      original_file : T_File; -- original file before copy
      original_file_name : Unbounded_String; -- original file name
      new_file : T_File; -- new file, copy of original file
   begin
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 2 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- folder to put the copy in
      destination_folder_parent := go_to_folder(current_directory, get_substring_to_string(parameters, 2), True);
      -- name of the new file / folder
      new_name := get_name_from_path(get_substring(parameters, 2));
      -- if this is a recursive cp
      if contains_option(options, 'r') then
         -- folder to copy
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
         -- create the new folder
         mkdir_command(current_directory, create_substrings, get_substrings(parameters, 2, 2));
         -- get the new folder pointer
         destination_folder := find_folder(destination_folder_parent, To_String(new_name));
         -- starting to copy the contents from source to new
         folder_deep_copy(source_folder, destination_folder);
      else
         -- folder to get the original file from
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
         -- get the original file
         original_file_name := get_name_from_path(get_substring(parameters, 1));
         original_file := find_file(source_folder, To_String(original_file_name));
         -- if the original file is null, it means it is not valid, raise an exception
         if original_file = null then
            raise Invalid_File_Error;
         end if;
         -- create the new file as a copy of original file
         new_file := clone(original_file, To_String(new_name), get_pwd(destination_folder_parent));
         -- add this new file to the destination folder, parent of the new file
         add_file(destination_folder_parent, new_file);
      end if;
   end cp_command;
   
   procedure mv_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      source_folder : T_Folder; -- the source folder containing the file
      destination_folder : T_Folder; -- the parent of the new file
      original_file_name : Unbounded_String; -- the original file name
      new_file_name : Unbounded_String; -- the new name of the file
      original_file : T_File; -- the original file
      new_file : T_File; -- the new file
   begin
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 2 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if this is a recursive move
      if contains_option(options, 'r') then
         -- execute a copy
         cp_command(current_directory, options, parameters);
         -- remove the old directory
         rm_command (current_directory, options, get_substrings(parameters, 1, 1));
      else
         -- go to source and destination folders from parameters
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
         destination_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 2), True);
         -- get original and new file names from parameters
         original_file_name := get_name_from_path(get_substring(parameters, 1));
         new_file_name := get_name_from_path(get_substring(parameters, 2));
         -- get the original file
         original_file := find_file(source_folder, To_String(original_file_name));
         -- if the original file is null, it means it is not valid, raise an exception
         if original_file = null then
            raise Invalid_File_Error;
         end if;
         -- clone the original file as a new file
         new_file := clone(original_file, To_String(new_file_name), get_pwd(destination_folder));
         -- add the new file to the destination folder
         add_file(destination_folder, new_file);
         -- delete the original file from the source folder
         del_file(source_folder, To_String(original_file_name));
      end if;
   end mv_command;
   
   procedure rm_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      parent_folder_deleted : T_Folder; -- parent folder of the deleted file / folder
      deleted_name : Unbounded_String; -- name of the deleted file / folder
   begin       
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- get deleted folder name from parameters 
      deleted_name := get_name_from_path(get_substring(parameters, 1));
      -- get deleted folder parent from parameters
      parent_folder_deleted := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      -- if this is a recursive rm
      if contains_option(options, 'r') then
         -- if no folder is found in the parent folder for the specified name, raise an exception
         if is_null(find_folder(parent_folder_deleted, To_String(deleted_name))) then
            raise Invalid_Folder_Error;
         end if;
         -- delete the folder of the specified name in the parent folder
         del_folder(parent_folder_deleted, To_String(deleted_name));
      else
         -- if no file is found in the parent folder for the specified name, raise an exception
         if find_file(parent_folder_deleted, To_String(deleted_name)) = null then
            raise Invalid_File_Error;
         end if;
         -- delete the file of the specified name in the parent folder
         del_file(parent_folder_deleted, To_String(deleted_name));
      end if;
   end rm_command;
   
   procedure tar_command(currentDirectory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      size: Integer;
      folder_to_tar: T_Folder;
      new_file : T_File;
      name: Unbounded_String;
   begin
      if(get_nb_substrings(options) > 0)then
         raise Not_Handled_Option_Error;
      end if;
      if(get_nb_substrings(parameters) > 1)then
         raise Wrong_Parameters_Number_Error;
      end if;
      if(get_substring_to_string(parameters, 1)'Length > 0)then
         folder_to_tar := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1));
      else
         folder_to_tar := currentDirectory;
      end if;
      size := calculate_size(folder_to_tar);
      name := (if is_root(folder_to_tar) then To_Unbounded_String("root") else To_Unbounded_String(get_name(folder_to_tar)));
      new_file := create(To_String(name) & ".tar", get_path(currentDirectory) & "/" & get_name(currentDirectory));
      set_size(new_file, size);
      add_file(currentDirectory, new_file);
   end tar_command;
   
   procedure clear_command is
   begin
      put(ASCII.ESC & "[2J");
   end clear_command;
   
   procedure help_command (options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a command is given, print the specific help of this command
      if get_nb_substrings(parameters) = 1 then
         print_specific_help(get_substring_to_string(parameters, 1));
      -- else, print the global help
      else
         print_global_help;
      end if;
   end help_command;

end P_Commands;
