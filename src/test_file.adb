with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Metadata; use P_Metadata;
with P_File; use P_File;

procedure test_file is
   file : T_File;
begin
   -- create
   put_line("Create:");
   put_line("Creating file.exe with rights 755 in /usr/bin, with some executable data");
   file := create("file.exe", (RWX, RX, RX), "/usr/bin", "thisissomeexecutabledata");
   if get_name(file) = "file.exe" then
      put_line("get_name(file) = ""file.exe""");
   else
      put_line("get_name(file) is incoherent");
   end if;
   if get_rights(file) = (RWX, RX, RX) then
      put_line("get_rights(file) = (RWX, RX, RX)");
   else
      put_line("get_rights(file) is incoherent");
   end if;
   if get_path(file) = "/usr/bin" then
      put_line("get_path(file) = ""/usr/bin""");
   else
      put_line("get_path(file) is incoherent");
   end if;
   if get_data(file) = "thisissomeexecutabledata" then
      put_line("get_data(file) = ""thisissomeexecutabledata""");
   else
      put_line("get_data(file) is incoherent");
   end if;
   new_line;
   
   -- set name & get name
   put_line("Set name & get name:");
   set_name(file, "file.dat");
   if get_name(file) = "file.dat" then
      put_line("get_name(file) = ""file.dat""");
   else
      put_line("get_name(file) is incoherent");
   end if;
   new_line;
   
   -- set rights & get rights
   put_line("Set rights & get rights:");
   set_rights(file, (RW, R, R));
   if get_rights(file) = (RW, R, R) then
      put_line("get_rights(file) = (RW, R, R)");
   else
      put_line("get_rights(file) is incoherent");
   end if;
   new_line;
   
   -- set size & get size
   put_line("Set size & get size:");
   set_size(file, 23568);
   if get_size(file) = 23568 then
      put_line("get_size(file) = 23568");
   else
      put_line("get_size(file) is incoherent");
   end if;
   new_line;
   
   -- set path & get path
   put_line("Set path and get path:");
   --set_path(file, "/home/n7");
   if get_path(file) = "/home/n7" then
      put_line("get_path(file) = ""/home/n7""");
   else
      put_line("get_path(file) is incoherent");
   end if;
   new_line;
   
   -- set data & get data
   put_line("Set data & get data:");
   set_data(file, "thisissomeotherdata");
   if get_data(file) = "thisissomeotherdata" then
      put_line("get_data(file) = ""thisissomeotherdata""");
   else
      put_line("get_data(file) is incoherent");
   end if;
   if get_size(file) = 19 then
      put_line("get_size(file) = 19 (size of ""thisissomeotherdata"")");
   else
      put_line("get_size(file) is incoherent");
   end if;
   new_line;
   
end test_file;
