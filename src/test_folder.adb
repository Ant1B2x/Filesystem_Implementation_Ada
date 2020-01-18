with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_Folder; use P_Folder;

procedure test_folder is
   root : T_Folder;
   folder : T_Folder; -- inside root
   folder_sibling : T_Folder; -- inside folder
   folder_data : T_Folder_Data;
begin
   -- get root
   put_line("Get root:");
   root := get_root;
   if get_name(root) = ""&FILE_SEPARATOR then
      put_line("get_name(root) = ""/""");
   else
      put_line("get_name(root) is incoherent");
   end if;
   if is_null(get_parent(root)) then
      put_line("get_parent(root) = null");
   else
      put_line("get_parent(root) is incoherent");
   end if;
   if get_rights(root) = (RWX, RX, RX) then
      put_line("get_rights(root) = (RWX, RX, RX)");
   else
      put_line("get_rights(root) is incoherent");
   end if;
   if get_size(root) = FOLDER_SIZE then
      put_line("get_size(root) = FOLDER_SIZE");
   else
      put_line("get_size(root) is incoherent");
   end if;
   if get_path(root) = "" then
      put_line("get_path(root) = """"");
   else
      put_line("get_path(root) is incoherent");
   end if;
   if is_empty(root) then
      put_line("is_empty(root) = True");
   else
      put_line("is_empty(root) is incoherent");
   end if;
   new_line;
   
   -- is root
   put_line("Is root:");
   if is_root(root) then
      put_line("is_root(root) = True");
   else
      put_line("is_root(root) is incoherent");
   end if;
   new_line;
   
   -- create
   put_line("Create:");
   folder := create("project", root, (RWX, RWX, RX));
   if get_name(folder) = "project" then
      put_line("get_name(folder) = ""project""");
   else
      put_line("get_name(folder) is incoherent");
   end if;
   if is_root(get_parent(folder)) then
      put_line("is_root(get_parent(folder))");
   else
      put_line("get_parent(folder) is incoherent");
   end if;
   if get_rights(folder) = (RWX, RWX, RX) then
      put_line("get_rights(folder) = (RWX, RWX, RX)");
   else
      put_line("get_rights(folder) is incoherent");
   end if;
   if get_size(folder) = FOLDER_SIZE then
      put_line("get_size(folder) = FOLDER_SIZE");
   else
      put_line("get_size(folder) is incoherent");
   end if;
   if get_path(folder) = "/" then
      put_line("get_path(folder) = ""/""");
   else
      put_line("get_path(folder) is incoherent");
   end if;
   new_line;
   
   -- is null
   put_line("Is null:");
   if not is_null(folder) then
      put_line("is_null(folder) = False");
   else
      put_line("is_null(folder) is incoherent");
   end if;
   
   -- set name & get name
   put_line("Set name & get name:");
   set_name(folder, "project_old");
   if get_name(folder) = "project_old" then
      put_line("get_name(folder) = ""project_old""");
   else
      put_line("get_name(folder) is incoherent");
   end if;
   new_line;
   
   -- set rights & get rights
   put_line("Set rights & get rights:");
   set_rights(folder, (RWX, NONE, NONE));
   if get_rights(folder) = (RWX, NONE, NONE) then
      put_line("get_rights(folder) = (RWX, NONE, NONE)");
   else
      put_line("get_rights(folder) is incoherent");
   end if;
   new_line;
   
   -- get size
   put_line("Get size:");
   if get_size(folder) = FOLDER_SIZE then
      put_line("get_size(folder) = FOLDER_SIZE");
   else
      put_line("get_size(folder) is incoherent");
   end if;
   new_line;
   
   -- set parent & get parent
   put_line("Set & get parent:");
   folder_sibling := create("drafts", root, (RWX, NONE, NONE));
   --set_parent(folder_sibling, folder);
   if get_name(get_parent(folder_sibling)) = get_name(folder) then
      put_line("get_name(get_parent(folder_sibling)) = get_name(folder)");
   else
      put_line("get_parent(folder_sibling) is incoherent");
   end if;
   new_line;
   
   -- is empty
   put_line("Is empty:");
   if is_empty(folder) then
      put_line("is_empty(folder) = True");
   else
      put_line("is_empty(folder) is incoherent");
   end if;
   new_line;
   
   -- errors
   
   --function calculate_path (folder : in T_Folder) return String;
   
   --function get_path (folder : in T_Folder) return String;
   
   --function get_folder (folder : in T_Folder; index : in Integer) return T_Folder;
   
   --function get_nb_folders (folder : in T_Folder) return Integer;
   
   --function find_folder (folder : in T_Folder; folder_name : in String) return T_Folder
   
   --function get_data (folder : in T_Folder) return T_Folder_Data;
   
   --procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data);
   
   --procedure add_folder (folder : in out T_Folder; new_folder : in T_Folder);
   
   --procedure del_folder (folder : in out T_Folder; folder_name : in String);
   
   --function get_file (folder : in T_Folder; index : in Integer) return T_File
   
   --function get_nb_files (folder : in T_Folder) return Integer;
   
   --function find_file (folder : in T_Folder; file_name : in String) return T_File
   
   --procedure add_file (folder : in out T_Folder; new_file : in T_File);
   
   --procedure del_file (folder : in out T_Folder; file_name : in String);
   
   
end test_folder;
