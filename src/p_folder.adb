package body p_folder is

   function create (name : in String; parent : in T_Folder) return T_Folder is
      folder : T_Folder;
      data : T_Folder_Data;
   begin
      folder := P_Folder_Tree.create(parent);
      
      data.nb_files := 0;
      data.metadata := P_Metadata.create(name, (RWX, RX, RX), 20);
      
      P_Folder_Tree.set_data(folder, data);
      return folder;
   end create;
   
   
   function create (name : in String; parent : in T_Folder; rights : in T_Rights) return T_Folder is
      folder : T_Folder;
      data : T_Folder_Data;
   begin
      folder := P_Folder_Tree.create(parent);
      
      data.nb_files := 0;
      data.metadata := P_Metadata.create(name, rights, 20);
      
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
   
   function get_parent (folder : in T_Folder) return T_Folder is
   begin
      return P_Folder_Tree.get_parent(folder);
   end get_parent;
   
   procedure set_parent (folder : in out T_Folder; parent : in T_Folder) is
   begin
      P_Folder_Tree.set_parent(folder, parent);
   end set_parent;
   
   function is_empty (folder : in T_Folder) return Boolean is
   begin
      return P_Folder_Tree.is_empty(folder);
   end is_empty;
   
   function get_folders (folder : in T_Folder) return T_Folders is
   begin
      return P_Folder_Tree.get_siblings(folder);
   end get_folders;
   
   function get_nb_folders (folder : in T_Folder) return Integer is
   begin
      return P_Folder_Tree.get_nb_siblings(folder);
   end get_nb_folders;
   
   procedure add_folder (folder : in out T_Folder; sibling_folder : in T_Folder) is
   begin
      P_Folder_Tree.add_sibling(folder, sibling_folder);
   end add_folder;
   
   procedure del_folder (folder : in out T_Folder; sibling_folder : in T_Folder) is
   begin
      P_Folder_Tree.del_sibling(folder, sibling_folder);
   end del_folder;
   
   function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder
     with Pre => folder_name'length <= LMAX_NAME;
   
   function get_files (folder : in T_Folder) return T_Files is
   begin
      return P_Folder_Tree.get_data(folder).files(1..P_Folder_Tree.get_data(folder).nb_files);
   end get_files;
   
   procedure add_file (folder : in out T_Folder; file : in T_File);
   
   procedure del_file (folder : in out T_Folder; file : in T_File);
   
   function find_file (folder : in T_Folder; file_name : in String) return T_File
     with Pre => file_name'length <= LMAX_NAME;
   
   
end p_folder;
