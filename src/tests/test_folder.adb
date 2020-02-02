with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;
with P_File; use P_File;
with P_Folder; use P_Folder;

procedure test_folder is
   root : T_Folder;
   folder : T_Folder; -- inside root
   folder_sibling : T_Folder; -- inside folder
   folder_sibling_bis : T_Folder; -- inside folder
   folder_data : T_Folder_Data; -- data of folder
   file : T_File; -- inside folder
begin
   -- get root
   put_line("Get root:");
   root := get_root;
   if get_name(root) = ""&FILE_SEPARATOR then
      put_line(ASCII.ESC & "[92m" & "get_name(root) = ""/""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if is_null(get_parent(root)) then
      put_line(ASCII.ESC & "[92m" & "get_parent(root) = null" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_parent(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_rights(root) = (RWX, RX, RX) then
      put_line(ASCII.ESC & "[92m" & "get_rights(root) = (RWX, RX, RX)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_size(root) = FOLDER_SIZE then
      put_line(ASCII.ESC & "[92m" & "get_size(root) = FOLDER_SIZE" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(root) = "" then
      put_line(ASCII.ESC & "[92m" & "get_path(root) = """"" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_pwd(root) = "/" then
      put_line(ASCII.ESC & "[92m" & "get_pwd(root) = ""/""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_pwd(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if is_empty(root) then
      put_line(ASCII.ESC & "[92m" & "is_empty(root) = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_empty(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- is root
   put_line("Is root:");
   if is_root(root) then
      put_line(ASCII.ESC & "[92m" & "is_root(root) = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_root(root) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- create
   put_line("Create:");
   folder := create("project", root, (RWX, RWX, RX));
   if get_name(folder) = "project" then
      put_line(ASCII.ESC & "[92m" & "get_name(folder) = ""project""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if is_root(get_parent(folder)) then
      put_line(ASCII.ESC & "[92m" & "is_root(get_parent(folder))" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_parent(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_name(get_folder(root, 1)) = get_name(folder) then
      put_line(ASCII.ESC & "[92m" & "get_name(get_folder(root, 1)) = get_name(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(get_folder(root, 1)) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_rights(folder) = (RWX, RWX, RX) then
      put_line(ASCII.ESC & "[92m" & "get_rights(folder) = (RWX, RWX, RX)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_size(folder) = FOLDER_SIZE then
      put_line(ASCII.ESC & "[92m" & "get_size(folder) = FOLDER_SIZE" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(folder) = "/" then 
      put_line(ASCII.ESC & "[92m" & "get_path(folder) = ""/""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_pwd(folder) = "/project" then
      put_line(ASCII.ESC & "[92m" & "get_pwd(folder) = ""/project""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_pwd(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- is null
   put_line("Is null:");
   if not is_null(folder) then
      put_line(ASCII.ESC & "[92m" & "is_null(folder) = False" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_null(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set name & get name
   put_line("Set name & get name:");
   set_name(folder, "project_old");
   if get_name(folder) = "project_old" then
      put_line(ASCII.ESC & "[92m" & "get_name(folder) = ""project_old""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set rights & get rights
   put_line("Set rights & get rights:");
   set_rights(folder, (RWX, NONE, NONE));
   if get_rights(folder) = (RWX, NONE, NONE) then
      put_line(ASCII.ESC & "[92m" & "get_rights(folder) = (RWX, NONE, NONE)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get size
   put_line("Get size:");
   if get_size(folder) = FOLDER_SIZE then
      put_line(ASCII.ESC & "[92m" & "get_size(folder) = FOLDER_SIZE" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;   
   
   -- is empty (true)
   put_line("Is empty (true):");
   if is_empty(folder) then
      put_line(ASCII.ESC & "[92m" & "is_empty(folder) = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_empty(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- create with parent & get parent
   put_line("Create with parent & get parent:");
   folder_sibling := create("drafts", folder, (RWX, NONE, NONE));
   if get_name(get_parent(folder_sibling)) = get_name(folder) then
      put_line(ASCII.ESC & "[92m" & "get_name(get_parent(folder_sibling)) = get_name(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_parent(folder_sibling) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- is empty (false)
   put_line("Is empty (false):");
   if not is_empty(folder) then
      put_line(ASCII.ESC & "[92m" & "is_empty(folder) = False, we just added folder_sibling" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "is_empty(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get nb folders
   put_line("Get nb folders:");
   if get_nb_folders(folder) = 1 then
      put_line(ASCII.ESC & "[92m" & "get_nb_folders(folder) = 1" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_nb_folders(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get folder
   put_line("Get folder:");
   if get_name(get_folder(folder, 1)) = get_name(folder_sibling) then
      put_line(ASCII.ESC & "[92m" & "get_name(get_folder(folder, 1)) = get_name(folder_sibling)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_folder(folder, 1) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- find folder
   put_line("Find folder:");
   if get_name(find_folder(folder, "drafts")) = "drafts" then
      put_line(ASCII.ESC & "[92m" & "get_name(find_folder(folder, ""drafts"")) = ""drafts""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(find_folder(folder, ""drafts"")) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if is_null(find_folder(folder, "abcd")) then
      put_line(ASCII.ESC & "[92m" & "is_null(find_folder(folder, ""abcd"") = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "find_folder(folder, ""abcd"") is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get path
   put_line("Get path:");
   if get_path(folder_sibling) = "/project_old" then
      put_line(ASCII.ESC & "[92m" & "get_path(folder_sibling) = ""/project_old""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(folder_sibling) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get pwd
   put_line("Get pwd:");
   if get_pwd(folder_sibling) = "/project_old/drafts" then
      put_line(ASCII.ESC & "[92m" & "get_pwd(folder_sibling) = ""/project_old/drafts""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_pwd(folder_sibling) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- del folder
   put_line("Del folder:");
   del_folder(folder, "drafts");
   if get_nb_folders(folder) = 0 then
      put_line(ASCII.ESC & "[92m" & "get_nb_folders(folder) = 0" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_nb_folders(folder) is incoherent, del_folder didn't work" & ASCII.ESC & "[0m");
   end if;
   new_line;

   -- add file & get file
   put_line("Add file & get file:");
   file := create("file_old.bak", (RW, R, R), "thisissomedata");
   add_file(folder, file);
   if get_name(get_file(folder, 1)) = get_name(file) then
      put_line(ASCII.ESC & "[92m" & "get_name(get_file(folder, 1)) = get_name(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_file(folder, 1) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(get_file(folder, 1)) = get_pwd(folder) then
      put_line(ASCII.ESC & "[92m" & "get_path(get_file(folder, 1)) = get_pwd(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_file(folder, 1) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get nb files
   put_line("Get nb files:");
   add_file(folder, create("executable_file.bak", (RWX, RX, RX), "thisissomeexecutabledata"));
   if get_nb_files(folder) = 2 then
      put_line(ASCII.ESC & "[92m" & "get_nb_files(folder) = 2" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_nb_files(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- del file
   put_line("Del file:");
   del_file(folder, "executable_file.bak");
   if get_nb_files(folder) = 1 then
      put_line(ASCII.ESC & "[92m" & "get_nb_files(folder) = 1, we still have one file left" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_nb_files(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- find file
   put_line("Find file:");
   if get_name(find_file(folder, "file_old.bak")) = get_name(file) then
      put_line(ASCII.ESC & "[92m" & "get_name(find_file(folder, ""file_old.bak"")) = get_name(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "find_file(folder, ""file_old.bak"") is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(find_file(folder, "file_old.bak")) = get_pwd(folder) then
      put_line(ASCII.ESC & "[92m" & "get_path(find_file(folder, ""file_old.bak"")) = get_pwd(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "find_file(folder, ""file_old.bak"") is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- has son with same name
   put_line("Has son with same name:");
   if has_son_with_this_name(folder, "file_old.bak") then
      put_line(ASCII.ESC & "[92m" & "has_son_with_this_name(folder, ""file_old.bak"") = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "has_son_with_this_name(folder, ""file_old.bak"") is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- has parent
   folder_sibling := create("sibling", folder);
   put_line("Has parent:");
   if has_as_parent(folder_sibling, folder) then
      put_line(ASCII.ESC & "[92m" & "has_parent(folder_sibling, folder) = True" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "has_parent(folder_sibling, folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get data
   put_line("Get data:");
   folder_data := get_data(folder);
   if get_name(folder_data.metadata) = get_name(folder) then
      put_line(ASCII.ESC & "[92m" & "get_name(folder_data.metadata) = get_name(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(folder_data.metadata) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_rights(folder_data.metadata) = get_rights(folder) then
      put_line(ASCII.ESC & "[92m" & "get_rights(folder_data.metadata) = get_rights(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(folder_data.metadata) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_size(folder_data.metadata) = get_size(folder) then
      put_line(ASCII.ESC & "[92m" & "get_size(folder_data.metadata) = get_size(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(folder_data.metadata) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(folder_data.metadata) = get_path(folder) then
      put_line(ASCII.ESC & "[92m" & "get_path(folder_data.metadata) = get_path(folder)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(folder_data.metadata) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set data
   put_line("Set data:");
   set_name(folder_data.metadata, "new_folder_name");
   set_data(folder, folder_data);
   if get_name(folder) = "new_folder_name" then
      put_line(ASCII.ESC & "[92m" & "get_name(folder) = ""new_folder_name""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(folder) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- raising Same_Name_Error when adding a folder
   put_line("Raising Same_Name_Error when adding a folder:");
   folder_sibling := create("common_folder_name", folder);
   begin
      folder_sibling_bis := create("common_folder_name", folder);
      put_line(ASCII.ESC & "[91m" & "Nothing raised, creating a folder should have raised Same_Name_Error" & ASCII.ESC & "[0m");
   exception
      when Same_Name_Error =>
         put_line(ASCII.ESC & "[92m" & "Same_Name_Error raised, you can't have two folders with the same name in the same folder" & ASCII.ESC & "[0m");
   end;
   new_line;
   
   -- raising Same_Name_Error when adding a file
   put_line("Raising Same_Name_Error when adding a file:");
   add_file(folder, create("common_file_name", (RW, R, R), "data"));
   begin
      add_file(folder, create("common_file_name", (RWX, RX, RX), "executabledata"));
      put_line(ASCII.ESC & "[91m" & "Nothing raised, add_file should have raised Same_Name_Error" & ASCII.ESC & "[0m");
   exception
      when Same_Name_Error =>
         put_line(ASCII.ESC & "[92m" & "Same_Name_Error raised, you can't have two files with the same name in the same folder" & ASCII.ESC & "[0m");
   end;
   new_line;
    
end test_folder;
