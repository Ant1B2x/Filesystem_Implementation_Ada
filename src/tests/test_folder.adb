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
   if get_name(get_folder(root, 1)) = get_name(folder) then
      put_line("get_name(get_folder(root, 1)) = get_name(folder)");
   else
      put_line("get_name(get_folder(root, 1)) is incoherent");
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
   new_line;
   
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
   
   -- is empty (true)
   put_line("Is empty (true):");
   if is_empty(folder) then
      put_line("is_empty(folder) = True");
   else
      put_line("is_empty(folder) is incoherent");
   end if;
   new_line;
   
   -- create with parent & get parent
   put_line("Create with parent & get parent:");
   folder_sibling := create("drafts", folder, (RWX, NONE, NONE));
   if get_name(get_parent(folder_sibling)) = get_name(folder) then
      put_line("get_name(get_parent(folder_sibling)) = get_name(folder)");
   else
      put_line("get_parent(folder_sibling) is incoherent");
   end if;
   new_line;
   
   -- is empty (false)
   put_line("Is empty (false):");
   if not is_empty(folder) then
      put_line("is_empty(folder) = False, we just added folder_sibling");
   else
      put_line("is_empty(folder) is incoherent");
   end if;
   new_line;
   
   -- get nb folders
   put_line("Get nb folders:");
   if get_nb_folders(folder) = 1 then
      put_line("get_nb_folders(folder) = 1");
   else
      put_line("get_nb_folders(folder) is incoherent");
   end if;
   new_line;
   
   -- get folder
   put_line("Get folder:");
   if get_name(get_folder(folder, 1)) = get_name(folder_sibling) then
      put_line("get_name(get_folder(folder, 1)) = get_name(folder_sibling)");
   else
      put_line("get_folder(folder, 1) is incoherent");
   end if;
   new_line;
   
   -- find folder
   put_line("Find folder:");
   if get_name(find_folder(folder, "drafts")) = "drafts" then
      put_line("get_name(find_folder(folder, ""drafts"")) = ""drafts""");
   else
      put_line("get_name(find_folder(folder, ""drafts"")) is incoherent");
   end if;
   if is_null(find_folder(folder, "abcd")) then
      put_line("is_null(find_folder(folder, ""abcd"") = True");
   else
      put_line("find_folder(folder, ""abcd"") is incoherent");
   end if;
   new_line;
   
   -- get path
   put_line("Get path:");
   put_line(get_path(folder_sibling));
   if get_path(folder_sibling) = FILE_SEPARATOR & get_name(folder) then
      put_line("get_path(folder_sibling) = FILE_SEPARATOR & get_name(folder)");
   else
      put_line("get_path(folder_sibling) is incoherent");
   end if;
   new_line;
   
   -- del folder
   put_line("Del folder:");
   del_folder(folder, "drafts");
   if get_nb_folders(folder) = 0 then
      put_line("get_nb_folders(folder) = 0");
   else
      put_line("get_nb_folders(folder) is incoherent, del_folder didn't work");
   end if;
   new_line;

   -- add file & get file
   put_line("Add file & get file:");
   file := create("file_old.bak", (RW, R, R), get_path(folder), "thisissomedata");
   add_file(folder, file);
   if get_file(folder, 1) = file then
      put_line("get_file(folder, 1) = file");
   else
      put_line("get_file(folder, 1) is incoherent");
   end if;
   new_line;
   
   -- get nb files
   put_line("Get nb files:");
   add_file(folder, create("executable_file.bak", (RWX, RX, RX), get_path(folder), "thisissomeexecutabledata"));
   if get_nb_files(folder) = 2 then
      put_line("get_nb_files(folder) = 2");
   else
      put_line("get_nb_files(folder) is incoherent");
   end if;
   new_line;
   
   -- del file
   put_line("Del file:");
   del_file(folder, "executable_file.bak");
   if get_nb_files(folder) = 1 then
      put_line("get_nb_files(folder) = 1, we still have one file left");
   else
      put_line("get_nb_files(folder) is incoherent");
   end if;
   new_line;
   
   -- find file
   put_line("Find file:");
   if find_file(folder, "file_old.bak") = file then
      put_line("find_file(folder, ""file_old.bak"") = file");
   else
      put_line("find_file(folder, ""file_old.bak"") is incoherent");
   end if;
   new_line;
   
   -- has son with same name
   put_line("Has son with same name:");
   if has_son_with_this_name(folder, "file_old.bak") then
      put_line("has_son_with_this_name(folder, ""file_old.bak"")");
   else
      put_line("has_son_with_this_name(folder, ""file_old.bak"") is incoherent");
   end if;
   new_line;
   
   -- get data
   put_line("Get data:");
   folder_data := get_data(folder);
   if get_name(folder_data.metadata) = get_name(folder) then
      put_line("get_name(folder_data.metadata) = get_name(folder)");
   else
      put_line("get_name(folder_data.metadata) is incoherent");
   end if;
   if get_rights(folder_data.metadata) = get_rights(folder) then
      put_line("get_rights(folder_data.metadata) = get_rights(folder)");
   else
      put_line("get_rights(folder_data.metadata) is incoherent");
   end if;
   if get_size(folder_data.metadata) = get_size(folder) then
      put_line("get_size(folder_data.metadata) = get_size(folder)");
   else
      put_line("get_size(folder_data.metadata) is incoherent");
   end if;
   if get_path(folder_data.metadata) = get_path(folder) then
      put_line("get_path(folder_data.metadata) = get_path(folder)");
   else
      put_line("get_path(folder_data.metadata) is incoherent");
   end if;
   new_line;
   
   -- set data
   put_line("Set data:");
   set_name(folder_data.metadata, "new_folder_name");
   set_data(folder, folder_data);
   if get_name(folder) = "new_folder_name" then
      put_line("get_name(folder) = ""new_folder_name""");
   else
      put_line("get_name(folder) is incoherent");
   end if;
   new_line;
   
   -- raising Same_Name_Error when adding a folder
   put_line("Raising Same_Name_Error when adding a folder:");
   folder_sibling := create("common_folder_name", folder);
   begin
      folder_sibling_bis := create("common_folder_name", folder);
      put_line("Nothing raised, creating a folder should have raised Same_Name_Error");
   exception
      when Same_Name_Error =>
         put_line("Same_Name_Error raised, you can't have two folders with the same name in the same folder");
   end;
   new_line;
   
   -- raising Same_Name_Error when adding a file
   put_line("Raising Same_Name_Error when adding a file:");
   add_file(folder, create("common_file_name", (RW, R, R), get_path(folder), "data"));
   begin
      add_file(folder, create("common_file_name", (RWX, RX, RX), get_path(folder), "executabledata"));
      put_line("Nothing raised, add_file should have raised Same_Name_Error");
   exception
      when Same_Name_Error =>
         put_line("Same_Name_Error raised, you can't have two files with the same name in the same folder");
   end;
   new_line;
   
   -- raising File_Not_Found_Error when finding file
--     put_line("Raising File_Not_Found_Error when finding file:");
--     begin
--        file := find_file(folder, "idonotexist");
--        put_line("Nothing raised, find_file should have raised File_Not_Found_Error");
--     exception
--        when File_Not_Found_Error =>
--           put_line("File_Not_Found_Error raised, folder doesn't contain ""idonotexist"" file");
--     end;
--     new_line;
   
   -- raising Folder_Not_Found_Error when finding folder
--     put_line("Raising Folder_Not_Found_Error when finding folder:");
--     begin
--        folder_sibling := find_folder(folder, "idonotexist");
--        put_line("Nothing raised, find_folder should have raised Folder_Not_Found_Error");
--     exception
--        when Folder_Not_Found_Error =>
--           put_line("Folder_Not_Found_Error raised, folder doesn't contain ""idonotexist"" folder");
--     end;
--     new_line;
    
end test_folder;