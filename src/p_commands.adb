package body P_Commands is

   procedure pwdCommand(arguments: T_Substrings; currentDirectory: T_Folder) is
      absolutePath: Unbounded_String;
   begin
      if(not is_root(currentDirectory))then
         Put_Line(calculate_path(currentDirectory) & FILE_SEPARATOR & get_name(currentDirectory));
      else
         Put_Line(get_name(currentDirectory));
      end if;
   end pwdCommand;
   
   procedure lsRCommand(precedingPath: Unbounded_String; currentDirectory: T_Folder)is
      -- Pour colorier : https://docs.adacore.com/gnatcoll-docs/terminals.html
      -- with GNATCOLL.Terminal;  use GNATCOLL.Terminal;
      -- Info.Set_Color (Standard_Output, Blue);
      -- Put_Line ("A blue on yellow line");
      -- Info.Set_Color (Standard_Output, Style => Reset_All);
      currentPath: Unbounded_String;
   begin
      currentPath := precedingPath & FILE_SEPARATOR & get_name(currentDirectory);
      Put_Line(To_String(currentPath));
      for i in 1.. get_nb_folders(currentDirectory) loop
         Put_Line(get_name(get_folder(currentDirectory,i)));
      end loop;
      for i in 1.. get_nb_files(currentDirectory) loop
         Put_Line(get_name(get_folder(currentDirectory,i)));
      end loop;
      for i in 1.. get_nb_folders(currentDirectory) loop
         lsRCommand(currentPath, get_folder(currentDirectory,i));
      end loop;
   end lsRCommand;
   
   procedure lsCommand(OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder)is
   begin
      if(get_substring_to_string(arguments,1) = "-r")then
         for i in 1.. get_nb_folders(currentDirectory) loop
            lsRCommand(To_Unbounded_String(""&FILE_SEPARATOR), get_folder(currentDirectory,i));
         end loop;
      else
         Put_Line("Actuel :" & get_name(currentDirectory));
         for i in 1.. get_nb_folders(currentDirectory) loop
            Put_Line("    Dossier => " & get_name(get_folder(currentDirectory,i)));
         end loop;
         
         for i in 1.. get_nb_files(currentDirectory) loop
            Put_Line("    Fichier => " & get_name(get_file(currentDirectory, i)));
         end loop;
      end if;
   end lsCommand;
   
   function get_folders_and_files(folder: T_Folder)return sons is
      allSons: sons(1..2*LMAX_STRING);
      index_global : Integer;
   begin
      index_global := 0;
      for i in 1.. get_nb_folders(folder) loop
         allSons(index_global).Name := To_Unbounded_String(get_name(get_folder(folder, i)));
         allSons(index_global).isFolder := True;
         index_global := index_global + 1;
      end loop;
      
      for i in 1.. get_nb_files(folder) loop
         allSons(index_global).Name := To_Unbounded_String(get_name(get_file(folder, i)));
         allSons(index_global).isFolder := False;
         index_global := index_global + 1;
      end loop;
      Sort (allSons);
      return allSons;
   end get_folders_and_files;
   
   
   
   procedure rmCommand(OptionTrue : Boolean;arguments: T_Substrings; currentDirectory: in out T_Folder)is
   begin
      if(OptionTrue)then
         del_folder(currentDirectory,get_name(find_folder(currentDirectory,get_substring_to_string(arguments,1))));
      else
         del_file(currentDirectory,get_name(find_file(currentDirectory,get_substring_to_string(arguments,1))));
      end if;
   end rmCommand;
   
   procedure pwdCommand(currentDirectory: T_Folder)is
   begin
      if(not is_root(currentDirectory))then
         Put_Line(calculate_path(currentDirectory) & FILE_SEPARATOR & get_name(currentDirectory));
      else
         Put_Line(get_name(currentDirectory));
      end if;
   end pwdCommand;
   
   procedure cdCommand(arguments: T_Substrings; currentDirectory: in out T_Folder) is
      folders: T_Substrings;
      current: T_Folder;
   begin
      folders := split_string(get_substring_to_string(arguments,1), FILE_SEPARATOR);
      
      if(get_nb_substrings(folders) > 0)then
         if(get_substring_to_string(folders, 1)(1) = FILE_SEPARATOR)then
            current := get_root;
         else
            current := currentDirectory;
         end if;
         currentDirectory := go_to_folder(original_directory => current,
                                          path               => get_substring_to_string(arguments,1));
      else
         Put_Line("You have to enter a parameter.");
      end if;
   end cdCommand;
   
   procedure mkdirCommand(arguments: T_Substrings; currentDirectory: in out T_Folder)is
      fils : T_Folder;
      parent: T_Folder;
      path: Unbounded_String;
      fils_name: Unbounded_String;
   begin
      if(get_nb_substrings(arguments) /= 1)then
         raise wrong_number_of_arguments;
      end if;
      if(get_nb_substrings(split_string(get_substring_to_string(arguments, 1), FILE_SEPARATOR)) > 1) then
         parent := go_to_folder(currentDirectory, get_substring_to_string(arguments, 1), True);
      else
         parent := currentDirectory;
      end if;
      Put_Line(get_substring_to_string(arguments, 1));
      Put_Line(Integer'Image(get_nb_substrings(split_string(get_substring_to_string(arguments, 1), FILE_SEPARATOR))));
      path := get_substring(arguments, 1);
      fils_name := get_substring(split_string(To_String(path), FILE_SEPARATOR), get_nb_substrings(split_string(To_String(path), FILE_SEPARATOR)));
      fils := create(To_String(fils_name), parent);
   end mkdirCommand;
   
   -- Vérifier un petit coup, parce que ça a été fait assez vite alors que j'étais fatigué, donc son fonctionnement aurait la note :
   -- random/20
   -- Donc voilà, autant essayer une fois et voir si ça marche.
   -- Mais après il faudra tout revérifier.
   procedure cpCommand(OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder) is
      source_folder: T_Folder;
      destination_folder: T_Folder;
      new_sibling: T_Folder;
   begin
      -- file to copy
      source_folder := go_to_folder(currentDirectory, get_substring_to_string(arguments, 1), True);
      -- file to put the copy in
      destination_folder := go_to_folder(currentDirectory, get_substring_to_string(arguments, 2), True);
      
      -- creating a clone of the original folder
      new_sibling := create(get_name(source_folder), destination_folder, get_rights(source_folder));
      
      -- starting to copy all its contents
      folder_deep_copy(new_sibling, destination_folder);
   end cpCommand;
   
   procedure mvCommand(arguments: T_Substrings; currentDirectory: in out T_Folder) is
      source_folder: T_Folder;
      original_name: Unbounded_String;
      destination_folder: T_Folder;
      new_name: Unbounded_String;
      file: T_File;
   begin
      original_name := get_substring(split_string(get_substring_to_string(arguments, 1), FILE_SEPARATOR), get_nb_substrings(split_string(get_substring_to_string(arguments, 1), FILE_SEPARATOR)));
      new_name := get_substring(split_string(get_substring_to_string(arguments, 2), FILE_SEPARATOR), get_nb_substrings(split_string(get_substring_to_string(arguments, 2), FILE_SEPARATOR)));
      source_folder := go_to_folder(currentDirectory, get_substring_to_string(arguments, 1), True);
      destination_folder := go_to_folder(currentDirectory, get_substring_to_string(arguments, 2), True);
      file := find_file(destination_folder, To_String(original_name));
      set_name(file, To_String(new_name));
      add_file(source_folder, file);
      del_file(currentDirectory, To_String(original_name));
   end mvCommand;
   
   procedure tarCommand(arguments: T_Substrings; currentDirectory: in out T_Folder)is
      size: Integer;
      folder_to_tar: T_Folder;
      new_file : T_File;
   begin
      folder_to_tar := go_to_folder(currentDirectory, get_substring_to_string(arguments, 1));
      size := calculate_size(go_to_folder(currentDirectory, get_substring_to_string(arguments, 1)));
      new_file := create(get_name(folder_to_tar) & ".tar", get_path(currentDirectory) & "/" & get_name(currentDirectory));
      set_size(new_file, size);
      add_file(currentDirectory, new_file);
   end tarCommand;
   
   procedure touchCommand(arguments: T_Substrings; currentDirectory: in out T_Folder)is
      file : T_File;
   begin
      file := create(get_substring_to_string(arguments, 1), calculate_path(currentDirectory) & get_name(currentDirectory));
      add_file(currentDirectory,file);
   end touchCommand;
   
   function "<" (L, R : sonRecord) return Boolean is
   begin
      if L.Name < R.Name then return True;
      elsif L.Name = R.Name then if(l.isFolder)then return False; else return True; end if;
      else return False;
      end if;
   end "<";
   
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
      put_line("help     show this menu");
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
            when help =>
               help_command;
               
         end case;
      exception
         when Constraint_Error =>
            put_line("The command you entered is invalid.");
            new_line;
            help_command;
      end;
   end help_command;
   
   procedure help_command (has_command : in Boolean; command : in String) is
   begin
      if has_command then
         help_command(command);
      else
         help_command;
      end if;
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
         -- check is the fisrt folder is the root folder => "/home/..." for example
         if(path(1) = FILE_SEPARATOR)then
            -- start for root if it's true
            current := get_root;
         else
            -- else, start from the current folder
            current := original_directory;            
         end if;
         -- case for mkdir, for example, can we want to stop at the penultimate of the path, and then create a folder.
         penultimate := (if(stop_at_penultimate)then 1 else 0);
         siblings := split_string(path, FILE_SEPARATOR);
         -- follow the path from the defined start
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
         Put_Line("You have to enter a parameter.");
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
      for i in 1..get_nb_files(folder_to_copy) loop
         original_file := get_file(folder_to_copy, i);
         new_file := create(get_name(original_file), get_rights(original_file), get_path(folder_parent_of_clone) & FILE_SEPARATOR & get_name(folder_parent_of_clone), get_data(original_file));
         set_size(new_file, get_size(new_file));
         add_file(folder_parent_of_clone, new_file);
      end loop;
      
      for i in 1..get_nb_folders(folder_to_copy) loop
         original_sibling := get_folder(folder_to_copy, i);
         new_sibling := create(get_name(original_sibling), folder_parent_of_clone, get_rights(original_file));
         folder_deep_copy(get_folder(folder_to_copy, i), new_sibling);
      end loop;
   end folder_deep_copy;

end P_Commands;
