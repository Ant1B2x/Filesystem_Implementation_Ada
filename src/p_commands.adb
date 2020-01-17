package body P_Commands is

   procedure pwdCommand(arguments: T_Substrings; currentDirectory: T_Folder) is
      absolutePath: Unbounded_String;
   begin
      if(not is_root(currentDirectory))then
         Put_Line(get_path(currentDirectory) & FILE_SEPARATOR & get_name(currentDirectory));
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
      Put_Line(get_name(currentDirectory));
   end pwdCommand;
   
   procedure cdCommand(arguments: T_Substrings; currentDirectory: T_Folder) is
      folders: T_Substrings;
      current: T_Folder;
   begin
      folders := split_string(get_substring_to_string(arguments,1), FILE_SEPARATOR);
      
      if(get_nb_substrings(folders) > 0)then
         if(get_substring_to_string(folders, 1)(1) = FILE_SEPARATOR)then
            current := get_root;
            for i in 1..get_nb_substrings(folders) loop
               if(not(is_null(find_folder(current, get_substring_to_string(folders, i)))))then
                  Put_Line("> The specified path is not valid");
               end if;
            end loop;
         else
            current := currentDirectory;
            for i in 1..get_nb_substrings(folders) loop
               if(not is_null(find_folder(current, get_substring_to_string(folders, i))))then
                  Put_Line("> The specified path is not valid");
               end if;
            end loop;
         end if;
      else
         Put_Line("You have to enter a parameter.");
      end if;
   end cdCommand;
   
   procedure mkdirCommand(arguments: T_Substrings; currentDirectory: in out T_Folder)is
      fils : T_Folder;
   begin
      fils := create(get_substring_to_string(arguments,1), currentDirectory);
      add_folder(currentDirectory,fils);
   end mkdirCommand;
   
   procedure cpCommand(OptionTrue : Boolean; arguments: T_Substrings; currentDirectory: T_Folder);
   procedure mvCommand(arguments: T_Substrings; currentDirectory: T_Folder);
   procedure tarCommand(arguments: T_Substrings; currentDirectory: T_Folder);
   
   procedure touchCommand(arguments: T_Substrings; currentDirectory: in out T_Folder)is
      file : T_File;
   begin
      file := create(get_substring_to_string(arguments,1), calculate_path(currentDirectory) & get_name(currentDirectory));
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
   
   function go_to_folder (original_directory: T_Folder; siblings: T_Substrings) return T_Folder is
      current: T_Folder;
   begin
      if(get_nb_substrings(siblings) > 0)then
         -- check is the fisrt folder is the root folder => "/home/..." for example
         if(get_substring_to_string(siblings, 1)(1) = FILE_SEPARATOR)then
            -- start for root if it's true
            current := get_root;
         else
            -- else, start from the current folder
            current := original_directory;            
         end if;
         -- follow the path from the defined start
         for i in 1..get_nb_substrings(siblings) loop
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

end P_Commands;
