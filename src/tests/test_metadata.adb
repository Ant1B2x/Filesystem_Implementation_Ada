with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;

procedure test_metadata is
   metadata : T_Metadata;
begin
   -- create
   put_line("Create:");
   metadata := create("file.exe", (RWX, RX, RX), 2284, "/usr/bin");
   put_line("Creating file.exe in /usr/bin with rights 755, size of 2284");
   if get_name(metadata) = "file.exe" then
      put_line("get_name(metadata) = ""file.exe""");
   else
      put_line("get_name(metadata) is incoherent");
   end if;
   if get_rights(metadata) = (RWX, RX, RX) then
      put_line("get_rights(metadata) = (RWX, RX, RX)");
   else
      put_line("get_rights(metadata) is incoherent");
   end if;
   if get_size(metadata) = 2284 then
      put_line("get_size(metadata) = 2284");
   else
      put_line("get_size(metadata) is incoherent");
   end if;
   if get_path(metadata) = "/usr/bin" then
      put_line("get_path(metadata) = ""/usr/bin""");
   else
      put_line("get_path(metadata) is incoherent");
   end if;
   new_line;
   
   -- set name & get name
   put_line("Set name & get name:");
   set_name(metadata, "file.dat");
   if get_name(metadata) = "file.dat" then
      put_line("get_name(metadata) = ""file.dat""");
   else
      put_line("get_name(metadata is incoherent");
   end if;
   new_line;
   
   -- set name with invalid character
   put_line("Set name with invalid character:");
   begin
      set_name(metadata, "tp/file");
      put_line("nothing raised, set_name should have raised Invalid_Character_Error");
   exception
      when Invalid_Character_Error =>
         put_line("Invalid_Character_Error raised, can't have '/' in name");
   end;
   begin
      set_name(metadata, "tp file");
      put_line("nothing raised, set_name should have raised Invalid_Character_Error");
   exception
      when Invalid_Character_Error =>
         put_line("Invalid_Character_Error raised, can't have ' ' in name");
   end;
   new_line;
   
   -- set rights & get rights
   put_line("Set rights & get rights:");
   set_rights(metadata, (RW, R, R));
   if get_rights(metadata) = (RW, R, R) then
      put_line("get_rights(metadata) = (RW, R, R)");
   else
      put_line("get_rights(metadata) is incoherent");
   end if;
   new_line;
   
   -- set size & get size
   put_line("Set size & get size:");
   set_size(metadata, 23568);
   if get_size(metadata) = 23568 then
      put_line("get_size(metadata) = 23568");
   else
      put_line("get_size(metadata) is incoherent");
   end if;
   new_line;
   
   -- set path & get path
   put_line("Set path & get path:");
   set_path(metadata, "/home/n7");
   if get_path(metadata) = "/home/n7" then
      put_line("get_path(metadata = ""/home/n7""");
   else
      put_line("get_path(metadata is incoherent");
   end if;
   new_line;
   
   -- create root
   put_line("Create root:");
   metadata := create_root;
   if get_name(metadata) = ""&FILE_SEPARATOR then
      put_line("get_name(metadata) = ""/""");
   else
      put_line("get_name(metadata) is incoherent, create_root may contain errors");
   end if;
   if get_rights(metadata) = (RWX, RX, RX) then
      put_line("get_rights(metadata) = (RWX, RX, RX)");
   else
      put_line("get_rights(metadata) is incoherent, create_root may contain errors");
   end if;
   if get_size(metadata) = FOLDER_SIZE then
      put_line("get_size(metadata) = FOLDER_SIZE");
   else
      put_line("get_size(metadata) is incoherent, create_root may contain errors");
   end if;
   if get_path(metadata) = "" then
      put_line("get_path(metadata) = """"");
   else
      put_line("get_path(metadata) = is incoherent, create_root may contain errors");
   end if;
   new_line;
   
end test_metadata;
