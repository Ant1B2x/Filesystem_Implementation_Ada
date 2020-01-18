package body P_Folder is
   
   function create_root return T_Folder is
      folder : T_Folder;
      data : T_Folder_Data;
   begin
      folder := P_Folder_Tree.create;
      
      data.files := P_Files.create;
      data.metadata := P_Metadata.create_root;
      
      P_Folder_Tree.set_data(folder, data);
      return folder;
   end create_root;
   
   function create (name : in String; parent : in out T_Folder) return T_Folder is
   begin
      return create (name, parent, (RWX, RX, RX));
   end create;
   
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights) return T_Folder is
      folder : T_Folder;
      data : T_Folder_Data;
   begin
      folder := P_Folder_Tree.create(parent);
      
      data.files := P_Files.create;
      data.metadata := P_Metadata.create(name, rights, FOLDER_SIZE, calculate_path(parent) );
      
      P_Folder_Tree.set_data(folder, data);
      return folder;
   end create;
 
   function get_name (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_name( P_Folder_Tree.get_data(folder).metadata );
   end get_name;
   
   procedure set_name (folder : in out T_Folder; name : in String) is
      data : T_Folder_Data;
   begin
      data := P_Folder_Tree.get_data(folder);
      P_Metadata.set_name(data.metadata, name);
      P_Folder_Tree.set_data(folder, data);
   end set_name;
   
   function get_rights (folder : in T_Folder) return T_Rights is
   begin
      return P_Metadata.get_rights( P_Folder_Tree.get_data(folder).metadata );
   end get_rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights) is
      data : T_Folder_Data;
   begin
      data := P_Folder_Tree.get_data(folder);
      P_Metadata.set_rights(data.metadata, rights);
      P_Folder_Tree.set_data(folder, data);
   end set_rights;
   
   function get_size (folder : in T_Folder) return Integer is
   begin
      return P_Metadata.get_size (P_Folder_Tree.get_data(folder).metadata );
   end get_size;
   
   function get_root return T_Folder is
   begin
      
      -- singleton pattern
      if ROOT = null then
         ROOT := create_root;
      end if;
      
      return ROOT;
      
   end get_root;
   
   function calculate_path (folder : in T_Folder) return String is
      absolute_path: Unbounded_String;
      current: T_Folder;
   begin
      
      absolute_path := To_Unbounded_String("");
      current := folder;
      while not is_null(get_parent(current)) and then not is_root(get_parent(current)) loop
         current := get_parent(current);
         absolute_path := get_name(current) & "/" & absolute_path;
      end loop;
      absolute_path := "/" & absolute_path;
      
      return To_String(absolute_path);
      
   end calculate_path;
   
   function get_path (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_path (P_Folder_Tree.get_data(folder).metadata );
   end get_path;
   
   procedure set_path (folder : in out T_Folder; path : in String) is
      data : T_Folder_Data;
   begin
      data := P_Folder_Tree.get_data(folder);
      P_Metadata.set_path(data.metadata, path);
      P_Folder_Tree.set_data(folder, data);
   end set_path;
   
   function get_parent (folder : in T_Folder) return T_Folder is
   begin
      return P_Folder_Tree.get_parent(folder);
   end get_parent;
   
   function is_empty (folder : in T_Folder) return Boolean is
   begin
      return get_nb_folders(folder) = 0 and get_nb_files(folder) = 0;
   end is_empty;
   
   function is_null (folder : in T_Folder) return Boolean is
   begin
      return P_Folder_Tree.is_null(folder);
   end is_null;
   
   function is_root (folder : in T_Folder) return Boolean is
   begin
      return get_parent(folder) = null;
   end is_root;
   
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder is
   begin
      return P_Folder_Tree.get_sibling(folder, index);
   end get_folder;
   
   function get_nb_folders (folder : in T_Folder) return Integer is
   begin
      return P_Folder_Tree.get_nb_siblings(folder);
   end get_nb_folders;
   
   -- T_Tree doit être en "protected" car on a besoin de savoir que c'est un pointeur
   function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder is
   begin
      for i in 1..get_nb_folders(folder) loop
         if get_name ( get_folder(folder, i) ) = folder_name then 
            return get_folder(folder, i);
         end if;
      end loop;
      return null;
   end find_folder;
   
   function get_data (folder : in T_Folder) return T_Folder_Data is
   begin
      return P_Folder_Tree.get_data(folder);
   end get_data;
   
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data) is
   begin
      P_Folder_Tree.set_data(folder, folder_data);
   end set_data;
   
   procedure add_folder (folder : in out T_Folder; new_folder : in out T_Folder) is
   begin
      
      -- check if an existing directory/file has the same name
      if find_folder(folder, get_name(new_folder) ) /= null
        or find_file(folder, get_name(new_folder) ) /= null then
         raise same_name_error;
      end if;
         
      P_Folder_Tree.add_sibling(folder, new_folder);
      
   end add_folder;
   
   procedure del_folder (folder : in out T_Folder; folder_name : in String) is
      folder_sibling : T_Folder;
   begin
      for i in 1..get_nb_folders(folder) loop
         if get_name(get_folder(folder, i)) = folder_name then
            folder_sibling := get_folder(folder, i);
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
   
   function find_file (folder : in T_Folder; file_name : in String) return T_File is
   begin
      for i in 1..get_nb_files(folder) loop
         if P_File.get_name(get_file(folder, i)) = file_name then
            return get_file(folder, i);
         end if;
      end loop;
      return null;
   end find_file;
   
   procedure add_file (folder : in out T_Folder; new_file : in T_File) is
      folder_data : T_Folder_Data;
   begin
      
      -- check if an existing directory/file has the same name
      if find_folder(folder, P_File.get_name(new_file) ) /= null
        or find_file(folder, P_File.get_name(new_file) ) /= null then
         raise same_name_error;
      end if;
      
      folder_data := get_data(folder);
      P_Files.add_value(folder_data.files, new_file);
      set_data(folder, folder_data);
      
   end add_file;
   
   procedure del_file (folder : in out T_Folder; file_name : in String) is
      folder_data : T_Folder_Data;
   begin
      folder_data := get_data(folder);
      for i in 1..P_Files.get_nb_values(folder_data.files) loop
         if P_File.get_name( P_Files.get_value(folder_data.files, i) ) = file_name then
            P_Files.del_value(folder_data.files, P_Files.get_value(folder_data.files, i));
         end if;
      end loop;
      set_data(folder, folder_data);
   end del_file;
   
end P_Folder;
