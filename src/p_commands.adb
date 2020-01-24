package body P_Commands is
   
   function get_pwd (current_directory : in T_Folder) return String is
   begin
      if(is_root(current_directory))then
         return ""&FILE_SEPARATOR;
      elsif(is_root(get_parent(current_directory)))then
         return FILE_SEPARATOR & get_name(current_directory);
      else
         return get_path(current_directory) & FILE_SEPARATOR & get_name(current_directory);
      end if;
   end get_pwd;
   
   procedure pwd_command (current_directory: in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if(get_nb_substrings(options) > 0)then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) > 0)then
         raise Wrong_Arguments_Number_Error;
      end if;
      put_line(get_pwd(current_directory) );
   end pwd_command;
   
   procedure ls_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      current: T_Folder;
   begin
      if(not only_handled_options(options, "-rmf"))then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) > 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      if(get_substring_to_string(parameters, 1)'Length > 0)then
         current := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      else
         current := current_directory;
      end if;
      if options_contain(options, 'r') then
         put_line(".:");
         display_folders_and_files(create_siblings_set(current));
         for i in 1.. get_nb_folders(current) loop
            ls_r_command(get_folder(current, i), To_Unbounded_String("."));
         end loop;
      else
         display_folders_and_files(create_siblings_set(current));
      end if;
   end ls_command;
   
   procedure rm_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      directory_to_del_from: T_Folder;
      name: Unbounded_String;
   begin       
      if(not only_handled_options(options, "-r"))then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) /= 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      name := get_name_from_path(get_substring(parameters, 1));
      directory_to_del_from := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      
      if options_contain(options, 'r') then
         begin
            del_folder(directory_to_del_from, get_name(find_folder(directory_to_del_from, To_String(name))));
         exception
               when Constraint_Error => Put_Line("No folder of that name");
         end;
      else
         begin
            del_file(directory_to_del_from, get_name(find_file(directory_to_del_from, To_String(name))));
         exception
            when Constraint_Error => Put_Line("No file of that name");
         end;
      end if;
      
   end rm_command;
   
   
   
   procedure cd_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if(get_nb_substrings(options) > 0)then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) > 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      currentDirectory := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1));
   exception
      when Invalid_Folder_Error =>
         Put_Line("Can not move to this directory. It does not exist.");
   end cd_command;
   
   procedure mkdir_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings)is
      fils : T_Folder;
      parent: T_Folder;
      fils_name: Unbounded_String;
   begin
      if(get_nb_substrings(options) > 0)then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) /= 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      
      parent := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1), True);
      fils_name := get_name_from_path(get_substring(parameters, 1));
      fils := create(To_String(fils_name), parent);
   exception
      when Invalid_Folder_Error =>
         put_line("cannot create directory '" & get_substring_to_string(parameters, 1) & "': No such file or directory");
      when Same_Name_Error =>
         Put_Line("cannot create directory '" & get_substring_to_string(parameters, 1) & "': File or directory with same name already exist");
   end mkdir_command;
   
   -- Vérifier un petit coup, parce que ça a été fait assez vite alors que j'étais fatigué, donc son fonctionnement aurait la note :
   -- random/20
   -- Donc voilà, autant essayer une fois et voir si ça marche.
   -- Mais après il faudra tout revérifier.
   procedure cp_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      source_folder: T_Folder;
      destination_folder: T_Folder;
      new_folder: T_Folder;
      original_file : T_File;
      new_file: T_File;
      original_path: Unbounded_String;
      original_name: Unbounded_String;
      new_path: Unbounded_String;
      new_name: Unbounded_String;
   begin
      if(not only_handled_options(options, "-r"))then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) /= 2)then
         raise Wrong_Arguments_Number_Error;
      end if;
      
      -- folder to get the copy from
      source_folder := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1), True);
      -- folder to put the copy in
      destination_folder := go_to_folder(currentDirectory, get_substring_to_string(parameters, 2), True);
      
      new_name := get_name_from_path(get_substring(parameters, 2));
      
      if(options_contain(options, 'r'))then
         mkdir_command(currentDirectory, get_substrings(parameters, 2, 2), create_substrings);
         
         new_folder := find_folder(destination_folder, To_String(new_name));
         
         -- starting to copy all its contents
         folder_deep_copy(source_folder, new_folder);
      else
         original_name := get_name_from_path(get_substring(parameters, 1));
         
         -- ? A remplacer par clone ?
         original_file := find_file(source_folder, To_String(original_name));
         new_file := create(To_String(new_name), get_rights(original_file), get_path(destination_folder) & FILE_SEPARATOR & get_name(destination_folder), get_data(original_file));
         set_size(new_file, get_size(original_file));
         add_file(destination_folder, new_file);
      end if;
   end cp_command;
   
   procedure mv_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      source_folder: T_Folder;
      original_name: Unbounded_String;
      destination_folder: T_Folder;
      new_name: Unbounded_String;
      file: T_File;
   begin
      if(not only_handled_options(options, "-r"))then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) /= 2)then
         raise Wrong_Arguments_Number_Error;
      end if;
      
      original_name := get_name_from_path(get_substring(parameters, 1));
      new_name := get_name_from_path(get_substring(parameters, 2));
      
      source_folder := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1), True);
      destination_folder := go_to_folder(currentDirectory, get_substring_to_string(parameters, 2), True);
      
      if(options_contain(options, 'r'))then
         cp_command(currentDirectory, options, parameters);
         rm_command (currentDirectory, options, parameters);
      else
         file := find_file(source_folder, To_String(original_name));
         
         del_file(source_folder, To_String(original_name));
         -- A remplacer par clone
         set_name(file, To_String(new_name));
         add_file(destination_folder, file);
      end if;
   end mv_command;
   
   procedure tar_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings)is
      size: Integer;
      folder_to_tar: T_Folder;
      new_file : T_File;
      name: Unbounded_String;
   begin
      if(get_nb_substrings(options) > 0)then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) > 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      folder_to_tar := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1));
      size := calculate_size(go_to_folder(currentDirectory, get_substring_to_string(parameters, 1)));
      name := (if is_root(folder_to_tar) then To_Unbounded_String("root") else To_Unbounded_String(get_name(folder_to_tar)));
      new_file := create(To_String(name) & ".tar", get_path(currentDirectory) & "/" & get_name(currentDirectory));
      set_size(new_file, size);
      add_file(currentDirectory, new_file);
   exception
      when Invalid_Folder_Error =>
         Put_Line("It is not possible to compress an unexisting folder");
   end tar_command;
   
   procedure touch_command(currentDirectory: in out T_Folder; options : in T_Substrings; parameters : in T_Substrings)is
      file : T_File;
      parent: T_Folder;
      file_name: Unbounded_String;
   begin
      if(get_nb_substrings(options) > 0)then
         raise Option_Not_Handled_Error;
      end if;
      if(get_nb_substrings(parameters) /= 1)then
         raise Wrong_Arguments_Number_Error;
      end if;
      if(get_nb_substrings(split_string(get_substring_to_string(parameters, 1), FILE_SEPARATOR)) > 1) then
         parent := go_to_folder(currentDirectory, get_substring_to_string(parameters, 1), True);
      else
         parent := currentDirectory;
      end if;
      file_name := get_name_from_path(get_substring(parameters, 1));
      file := create(To_String(file_name), get_path(parent) & get_name(parent));
      add_file(parent,file);
   end touch_command;
   
   function "<" (L, R : in T_R_Sibling) return Boolean is
   begin
      if L.name < R.name then return True;
      elsif L.name = R.name then if l.is_folder then return False; else return True; end if;
      else return False;
      end if;
   end "<";
   
   procedure clear_command is
   begin
      put(ESC & "[2J");
   end clear_command;
   
   procedure help_command (options: t_Substrings; parameters : in T_Substrings) is
   begin
      if get_nb_substrings(options) > 0 then
         raise Option_Not_Handled_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Arguments_Number_Error;
      end if;
      
      if get_nb_substrings(parameters) = 1 then
         help_command(get_substring_to_string(parameters, 1));
      else
         help_command;
      end if;
   end help_command;
   
     
   
   -- private functions & procedures
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String) is
      current_path: Unbounded_String;
   begin
      current_path := preceding_path & FILE_SEPARATOR & get_name(current_directory);
      new_line;
      put_line(To_String(current_path) & ":");
      
      display_folders_and_files(create_siblings_set(current_directory));
      for i in 1.. get_nb_folders(current_directory) loop
         ls_r_command(get_folder(current_directory,i), current_path);
      end loop;
   end ls_r_command;
   
   procedure help_command is
   begin
      put_line("Available commands:");
      put_line("pwd      print name of current/working directory");
      put_line("cd       change the working directory");
      put_line("ls       list directory contents");
      put_line("mkdir    make directories");
      put_line("touch    create empty file");
      put_line("cp       copy files and directories"); -- not sure
      put_line("mv       move (rename) files"); -- not sure
      put_line("rm       remove files or directories");
      put_line("tar      archive a file");
      put_line("clear    clear the terminal screen");
      put_line("help     show this menu");
      put_line("exit     cause the shell to exit");
      new_line;
      put_line("You can get more help by specifying a command.");
      put_line("For example, try 'help ls'.");
   end help_command;

   procedure help_command (command : in String) is
   begin
      begin
         case encoded_commands'Value(command) is
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
               put_line("  mv SOURCE DEST");
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
               put_line("  tar - archive a file");
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
               help_command;
         end case;
      exception
         when Constraint_Error =>
            put_line("No help entry for this command.");
            new_line;
            help_command;
      end;
   end help_command;
   
   
   function go_to_folder (original_directory: in T_Folder; path: in String) return T_Folder is
   begin
      return go_to_folder(original_directory, path, False);
   end go_to_folder;
   
   function go_to_folder (original_directory: in T_Folder; path: in String; stop_at_penultimate: in Boolean) return T_Folder is
      current: T_Folder;
      siblings: T_Substrings;
      penultimate: Integer;
   begin
      if(path'Length > 0)then
         -- Check is the fisrt folder is the root folder => "/home/..." for example
         if(path(path'First) = FILE_SEPARATOR)then
            -- Start for root if it's true
            current := get_root;
         else
            -- Else, start from the current folder
            current := original_directory;            
         end if;
         -- Case for mkdir, for example, can we want to stop at the penultimate of the path, and then create a folder.
         penultimate := (if(stop_at_penultimate)then 1 else 0);
         siblings := split_string(path, FILE_SEPARATOR);
         -- Follow the path from the defined start
         for i in 1..get_nb_substrings(siblings) - penultimate loop
            -- If the following sibling doesn't exist, raise an exception, otherwise, take the next sibling as current
            if(not is_null(find_folder(current, get_substring_to_string(siblings, i))))then
               current := find_folder(current, get_substring_to_string(siblings, i));
            else
               raise invalid_folder_error;
            end if;
         end loop;
         return current;
      else
         Put_Line("BOUH !");
         raise Wrong_Arguments_Number_Error;
         return null;
      end if;
   end go_to_folder;
   
   function calculate_size(folder: T_Folder) return Integer is
      current_folder_size: Integer;
   begin
      current_folder_size := FOLDER_SIZE;
      for i in 1..get_nb_files(folder) loop
         current_folder_size := current_folder_size + get_size(get_file(folder, i));
      end loop;
      
      for i in 1..get_nb_folders(folder) loop
         current_folder_size := current_folder_size + calculate_size(get_folder(folder, i));
      end loop;
      return current_folder_size;
   end calculate_size;
   
   procedure folder_deep_copy(folder_to_copy: T_Folder; folder_parent_of_clone: in out T_Folder) is
      original_file : T_File;
      new_file: T_File;
      original_sibling: T_Folder;
      new_sibling: T_Folder;
   begin
      -- Put_Line("Avant copie fichiers");
      for i in 1..get_nb_files(folder_to_copy) loop
         original_file := get_file(folder_to_copy, i);
         new_file := create(get_name(original_file), get_rights(original_file), get_path(folder_parent_of_clone) & FILE_SEPARATOR & get_name(folder_parent_of_clone), get_data(original_file));
         set_size(new_file, get_size(new_file));
         add_file(folder_parent_of_clone, new_file);
      end loop;
      -- Put_Line("Avant copie dossier");
      for i in 1..get_nb_folders(folder_to_copy) loop
         original_sibling := get_folder(folder_to_copy, i);
         new_sibling := create(get_name(original_sibling), folder_parent_of_clone, get_rights(original_sibling));
         -- Put_Line("Relance deep_copy");
         folder_deep_copy(get_folder(folder_to_copy, i), new_sibling);
      end loop;
   end folder_deep_copy;
   
   function create_siblings_set (directory : in T_Folder) return T_Siblings_Set is
      siblings_set : T_Siblings_Set;
      new_element : T_R_Sibling;
   begin
      for i in 1.. get_nb_folders(directory) loop
         new_element.name := To_Unbounded_String(get_name(get_folder(directory, i)));
         new_element.is_folder := True;
         siblings_set.insert(new_element);
      end loop;
      
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
         if sibling.is_folder then
            put(ESC & "[95m");
            put(To_String(sibling.name) & "  ");
            put(ESC & "[0m");
         else
            put(To_String(sibling.name) & "  ");
         end if;
      end loop;
      new_line;
   end display_folders_and_files;
   
   procedure get_options_parameter_and_options(arguments: in T_Substrings; options: in out T_Substrings; parameters: in out T_Substrings) is
   begin
      -- For every argument
      for i in 1..get_nb_substrings(arguments) loop
         -- If it starts with a '-', then it's an option
         if get_substring_to_string(arguments, i)(1) = '-' then
            -- If we only have '-', we raise an error
            if(get_substring_to_string(arguments, i)'Length < 2)then
               raise invalid_option_error;
            end if;
            -- We start at 2 because we need to get all options, and not the '-' character
            for j in 2..get_substring_to_string(arguments, i)'Length loop
               -- We have to transform it into a string, because the (j) part return a Character, and we add it to the options array
               add_substring(options, get_substring_to_string(arguments, i)(j) & "");
            end loop;
         else
            -- Else it's an argument and we add it to the arguments array
            add_substring(parameters, get_substring_to_string(arguments, i));
         end if;
      end loop;
   end get_options_parameter_and_options;
   
   function get_options(options_as_string: in String) return T_Substrings is
      splited_option: T_Substrings;
      options: T_Substrings;
   begin
      options := create_substrings;
      splited_option := split_string(options_as_string, '-');
      for i in 1.. get_nb_substrings(splited_option) loop
         -- If there is multiple option wrote after a '-' (like '-rf')
         if(get_substring_to_string(splited_option, i)'Length > 1)then
            -- We add each one of them to the options
            for j in 1.. get_substring_to_string(splited_option, i)'Length loop
               add_substring(options, get_substring_to_string(splited_option, i)(j) & "");
            end loop;
         else
            -- Else we add the option to the options
            add_substring(options, get_substring_to_string(splited_option, i)(1) & "");
         end if;
      end loop;
      return options;
   end get_options;
   
   function options_contain(options: in T_Substrings; option: in Character) return Boolean is
   begin
      for i in 1..get_nb_substrings(options) loop
         if(get_substring_to_string(options, i)(1) = option)then
            return True;
         end if;
      end loop;
      return False;
   end options_contain;
   
   function only_handled_options(options: in T_Substrings; options_handled: in String) return Boolean is
      options_handled_substrings: T_Substrings;
   begin
      options_handled_substrings := get_options(options_handled);
      for i in 1..get_nb_substrings(options) loop
         if(not options_contain(options_handled_substrings, get_substring_to_string(options, i)(1)))then
            return False;
         end if;
      end loop;
      return True;
   end only_handled_options;
   
   -- Here we chose to use Unbounded_String because we have to declare the name return, and we don't know his length
   function get_name_from_path(path: Unbounded_String) return Unbounded_String is
   begin
      return get_substring(split_string(To_String(path), FILE_SEPARATOR), get_nb_substrings(split_string(To_String(path), FILE_SEPARATOR)));
   end get_name_from_path;

end P_Commands;
