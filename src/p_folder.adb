package body P_Folder is
  
   function create_root return T_Folder is
      folder : T_Folder; -- the returned root folder
      data : T_Folder_Data; -- the data of the root folder
   begin
      -- initialize the folder tree
      folder := P_Folder_Tree.create;
      -- initialize the file array
      data.files := P_Files.create;
      -- initialize the folder metadata
      data.metadata := P_Metadata.create_root;
      -- set the folder data on the folder
      P_Folder_Tree.set_data(folder, data);
      -- return the root folder
      return folder;
   end create_root;
   
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights) return T_Folder is
      folder : T_Folder; -- the returned new folder
      data : T_Folder_Data; -- the data of the returned new folder
   begin
      -- if the parent contains a folder or a file with the same name than the new folder, raise an Exception
      if has_son_with_this_name(parent, name) then
         raise Same_Name_Error;
      end if;
      -- if the parent directory is already full of folders, raise an Exception
      if get_nb_folders(parent) = NMAX_VALUES then
         raise Full_Folder_Error;
      end if;
      -- initialize the folder tree
      folder := P_Folder_Tree.create(parent);
      -- initialize the file array
      data.files := P_Files.create;
      -- initialize the folder metadata, FOLDER_SIZE is set automatically and is equal to 10Ko, the absolute path is calculated
      data.metadata := P_Metadata.create(name, rights, FOLDER_SIZE, calculate_path(folder));
      -- set the folder data on the folder
      P_Folder_Tree.set_data(folder, data);
      -- return the new folder
      return folder;
   end create;
   
   function create (name : in String; parent : in out T_Folder) return T_Folder is
   begin
      -- call the already developped create function, with rights 755
      return create (name, parent, (RWX, RX, RX));
   end create;
 
   function get_name (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_name(P_Folder_Tree.get_data(folder).metadata);
   end get_name;
   
   procedure set_name (folder : in out T_Folder; name : in String) is
      data : T_Folder_Data; -- data of the folder
   begin
      -- we should do it like this because T_Folder_Data is private
      -- get the data of the folder
      data := P_Folder_Tree.get_data(folder);
      -- modify the name in the folder data
      P_Metadata.set_name(data.metadata, name);
      -- set the new data of the folder
      P_Folder_Tree.set_data(folder, data);
   end set_name;
   
   function get_rights (folder : in T_Folder) return T_Rights is
   begin
      return P_Metadata.get_rights(P_Folder_Tree.get_data(folder).metadata);
   end get_rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights) is
      data : T_Folder_Data; -- data of the folder
   begin
      -- we should do it like this because T_Folder_Data is private
      -- get the data of the folder
      data := P_Folder_Tree.get_data(folder);
      -- modify the rights in the folder data
      P_Metadata.set_rights(data.metadata, rights);
      -- set the new data of the folder
      P_Folder_Tree.set_data(folder, data);
   end set_rights;
   
   function get_size (folder : in T_Folder) return Integer is
   begin
      return P_Metadata.get_size (P_Folder_Tree.get_data(folder).metadata);
   end get_size;
   
   function get_root return T_Folder is
   begin
      -- singleton pattern
      -- if root does not exist, initialize it
      if ROOT = null then
         ROOT := create_root;
      end if;
      -- return root anyway
      return ROOT;
   end get_root;
   
   function calculate_path (folder : in T_Folder) return String is
      absolute_path : Unbounded_String; -- absolute path from root to folder
      current : T_Folder; -- current folder
   begin
      -- if the parent is root, it's a specific case, return "/"
      if is_root(get_parent(folder)) then
         return ""&FILE_SEPARATOR;
      end if;
      -- initialize absolute path and current folder
      absolute_path := To_Unbounded_String("");
      current := folder;
      -- while the current parent is not null and the current parent is not root
      while not is_null(get_parent(current)) and then not is_root(get_parent(current)) loop
         -- current becomes its parent
         current := get_parent(current);
         -- add "/" + name of current at the beggining of absolute path
         absolute_path := FILE_SEPARATOR & get_name(current) & absolute_path;
      end loop;
      -- return absolute calculated path
      return To_String(absolute_path);
   end calculate_path;
   
   function get_path (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_path (P_Folder_Tree.get_data(folder).metadata);
   end get_path;
   
   function get_pwd (folder : in T_Folder) return String is
   begin
      -- if the current directory is root
      if is_root(folder) then 
         -- return "" & "/"
         return get_path(folder) & FILE_SEPARATOR; 
      -- if the current directory is a root subdirectory
      elsif is_root(get_parent(folder)) then 
         -- return "/" & name
         return get_path(folder) & get_name(folder);
      -- in every other directory
      else
         -- return path & "/" & name
         return get_path(folder) & FILE_SEPARATOR & get_name(folder);
      end if;
   end get_pwd;
   
   function get_parent (folder : in T_Folder) return T_Folder is
   begin
      return P_Folder_Tree.get_parent(folder);
   end get_parent;
   
   function is_empty (folder : in T_Folder) return Boolean is
   begin
      -- return True if there is no folder and no file in the given folder
      return get_nb_folders(folder) = 0 and get_nb_files(folder) = 0;
   end is_empty;
   
   function is_null (folder : in T_Folder) return Boolean is
   begin
      return P_Folder_Tree.is_null(folder);
   end is_null;
   
   function is_root (folder : in T_Folder) return Boolean is
   begin
      -- in our implementation, only root has a null parent
      return get_parent(folder) = null;
   end is_root;
   
   function are_the_same(folder1 : T_Folder; folder2 : T_Folder) return Boolean is
   begin
      -- if the pwd of folders is equal, that means they are the same
      return get_pwd(folder1) = get_pwd(folder2);
   end are_the_same;
   
   function get_data (folder : in T_Folder) return T_Folder_Data is
   begin
      return P_Folder_Tree.get_data(folder);
   end get_data;
   
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data) is
   begin
      P_Folder_Tree.set_data(folder, folder_data);
   end set_data;
   
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder is
   begin
      return P_Folder_Tree.get_sibling(folder, index);
   end get_folder;
   
   function get_nb_folders (folder : in T_Folder) return Integer is
   begin
      return P_Folder_Tree.get_nb_siblings(folder);
   end get_nb_folders;
   
   function find_folder (current_folder : in T_Folder; folder_name : in String) return T_Folder is
   begin
      -- if the folder name is ".", return the current folder itself
      if folder_name = "." then
         return current_folder;
      end if;
      -- if the folder name is ".."
      if folder_name = ".." then
         -- if the current folder is root, return itself
         if is_root(current_folder) then
            return current_folder;
         -- else, return the folder parent
         else
            return get_parent(current_folder);
         end if;
      end if;
      -- for all sub-folders
      for i in 1..get_nb_folders(current_folder) loop
         -- if the sub-folder name is equal to the given folder_name
         if get_name ( get_folder(current_folder, i) ) = folder_name then
            -- return the sub-folder
            return get_folder(current_folder, i);
         end if;
      end loop;
      -- anyway, return null
      return null;
   end find_folder;
   
   procedure del_folder (folder : in out T_Folder; folder_name : in String) is
      folder_sibling : T_Folder; -- the potentially deleted sub-folder
   begin
      -- for all sub-folders
      for i in 1..get_nb_folders(folder) loop
         -- if the sub-folder name is equal to the given folder_name
         if get_name(get_folder(folder, i)) = folder_name then
            -- get the sub-folder using get_folder
            folder_sibling := get_folder(folder, i);
            -- deleted the sub-folder in the folder tree
            P_Folder_Tree.del_sibling(folder, folder_sibling);
         end if;
      end loop;
   end del_folder;
   
   function get_file (folder : in T_Folder; index : in Integer) return T_File is
   begin
      return P_Files.get_value( P_Folder_Tree.get_data(folder).files, index);
   end get_file;
   
   function get_nb_files (folder : in T_Folder) return Integer is
   begin
      return P_Files.get_nb_values (P_Folder_Tree.get_data(folder).files);
   end get_nb_files;
   
   function find_file (current_folder : in T_Folder; file_name : in String) return T_File is
   begin
      -- for all files
      for i in 1..get_nb_files(current_folder) loop
         -- if the file name is equal to the given file_name
         if P_File.get_name(get_file(current_folder, i)) = file_name then
            -- return the file
            return get_file(current_folder, i);
         end if;
      end loop;
      -- anyway, return null
      return null;
   end find_file;
   
   procedure add_file (folder : in out T_Folder; file : in T_File) is
      folder_data : T_Folder_Data; -- the data of the folder
      new_file : T_File; -- new file to add in the folder, clone of file parameter
   begin
      -- if the parent contains a folder or a file with the same name than the new file, raise an Exception
      if has_son_with_this_name(folder, P_File.get_name(file)) then
         raise Same_Name_Error;
      end if;
      -- if the parent directory is already full of files, raise an Exception
      if get_nb_files(folder) = NMAX_VALUES then
         raise Full_Folder_Error;
      end if;
      -- get the data of the folder
      folder_data := get_data(folder);
      -- clone the file parameter, change its path to the pwd of the folder
      new_file := clone(file, get_pwd(folder));
      -- add the new file to the files array of the folder
      P_Files.add_value(folder_data.files, new_file);
      -- set the modified data of the folder
      set_data(folder, folder_data);
   end add_file;
   
   procedure del_file (folder : in out T_Folder; file_name : in String) is
      folder_data : T_Folder_Data; -- the data of the folder
   begin
      -- get the data of the folder
      folder_data := get_data(folder);
      -- for all files
      for i in 1..P_Files.get_nb_values(folder_data.files) loop
         -- if the file name is equal to the given file name
         if P_File.get_name( P_Files.get_value(folder_data.files, i) ) = file_name then
            -- delete the file
            P_Files.del_value(folder_data.files, P_Files.get_value(folder_data.files, i));
         end if;
      end loop;
      -- set the modified data of the folder
      set_data(folder, folder_data);
   end del_file;
   
   function has_son_with_this_name(folder: T_Folder; name: String) return Boolean is
   begin
      -- return true if a folder or a file is found for the given name
      return find_folder(folder, name) /= null or find_file(folder, name) /= null;
   end has_son_with_this_name;
   
   function has_parent(folder : in T_Folder; supposed_parent : in T_Folder) return Boolean is
      current : T_Folder; -- current folder
   begin
      -- set the current to folder
      current := folder;
      -- while current is not root and current is different to supposed parent
      while not is_root(current) and not are_the_same(supposed_parent, current) loop
         -- current becomes its parent
         current := get_parent(current);
      end loop;
      -- finally, return True if supposed parent is equal to current, False otherwise
      return are_the_same(supposed_parent, current);
   end has_parent;
   
end P_Folder;
