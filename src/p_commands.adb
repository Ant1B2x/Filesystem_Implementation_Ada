package body P_Commands is

   procedure pwdCommand(firstParameter: String; currentDirectory: T_Folder) is
      absolutePath: Unbounded_String;
   begin
      Put_Line(">" & get_path(currentDirectory) & "/" & get_name(currentDirectory));
   end pwdCommand;
   
   procedure lsRCommand(precedingPath: Unbounded_String; currentDirectory: T_Folder)is
      -- Pour colorier : https://docs.adacore.com/gnatcoll-docs/terminals.html
      -- with GNATCOLL.Terminal;  use GNATCOLL.Terminal;
      -- Info.Set_Color (Standard_Output, Blue);
      -- Put_Line ("A blue on yellow line");
      -- Info.Set_Color (Standard_Output, Style => Reset_All);
      currentPath: Unbounded_String;
   begin
      currentPath := precedingPath & "/" & get_name(currentDirectory);
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
   
   procedure lsCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder)is
   begin
      if(firstParameter = "-r")then
         for i in 1.. get_nb_folders(currentDirectory) loop
            lsRCommand(To_Unbounded_String("/"), get_folder(currentDirectory,i));
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
      return sons;
   end get_folders_and_files;
   
   
   
   procedure rmCommand(OptionTrue : Boolean;firstParameter: String; currentDirectory: in out T_Folder)is
   begin
      if(OptionTrue)then
         del_folder(currentDirectory,get_name(find_folder(currentDirectory,firstParameter)));
      else
         del_file(currentDirectory,get_name(find_file(currentDirectory,firstParameter)));
      end if;
   end rmCommand;
   
   procedure pwdCommand(currentDirectory: T_Folder)is
   begin
      Put_Line(get_name(currentDirectory));
   end pwdCommand;
   
   procedure cdCommand(firstParameter: String; currentDirectory: T_Folder) is
      folders: T_Substrings;
      current: T_Folder;
   begin
      folders := split_string(firstParameter, '/');
      
      if(get_nb_substrings(folders) > 0)then
         if(get_substring_to_string(folders, 1) = "/")then
            current := get_root;
            for i in 1..get_nb_substrings(folders) loop
               if(find_folder(current, get_substring_to_string(folders, i)) /= null)then
                  Put_Line("> The specified path is not valid");
               end if;
            end loop;
         else
            current := currentDirectory;
            for i in 1..get_nb_substrings(folders) loop
               if(find_folder(current, get_substring_to_string(folders, i)) /= null)then
                  Put_Line("> The specified path is not valid");
               end if;
            end loop;
         end if;
      else
         Put_Line("> You have to enter a parameter.");
      end if;
   end cdCommand;
   
   procedure mkdirCommand(firstParameter: String; currentDirectory: in out T_Folder)is
      fils : T_Folder;
   begin
      fils := create(firstParameter, currentDirectory);
      add_folder(currentDirectory,fils);
   end mkdirCommand;
   
   procedure cpCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure mvCommand(firstParameter: String; secondParameter: String; currentDirectory: T_Folder);
   procedure tarCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure touchCommand(firstParameter: String; currentDirectory: in out T_Folder)is
      file : T_File;
   begin
      file := create(firstParameter, calculate_path(currentDirectory) & get_name(currentDirectory));
      add_file(currentDirectory,file);
   end touchCommand;
   
   function "<" (L, R : sonRecord) return Boolean is
   begin
      if L.Name < R.Name then return True;
      elsif L.Name = R.Name then if(l.isFolder)then return False; else return True; end if;
      else return False;
      end if;
   end "<";

end P_Commands;
