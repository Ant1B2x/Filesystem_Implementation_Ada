with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Metadata; use P_Metadata;
with P_File; use P_File;

procedure test_file is
   file : T_File;
   file_bis : T_File;
begin
   -- create
   put_line("Create:");
   put_line("Creating file.exe with rights 755 and some executable data");
   file := create("file.exe", (RWX, RX, RX), "thisissomeexecutabledata");
   if get_name(file) = "file.exe" then
      put_line(ASCII.ESC & "[92m" & "get_name(file) = ""file.exe""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_rights(file) = (RWX, RX, RX) then
      put_line(ASCII.ESC & "[92m" & "get_rights(file) = (RWX, RX, RX)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_path(file) = "" then
      put_line(ASCII.ESC & "[92m" & "get_path(file) = """"" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_data(file) = "thisissomeexecutabledata" then
      put_line(ASCII.ESC & "[92m" & "get_data(file) = ""thisissomeexecutabledata""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_data(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- get path
   put_line("Get path:");
   if get_path(file) = "" then
      put_line(ASCII.ESC & "[92m" & "get_path(file) = """"" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set name & get name
   put_line("Set name & get name:");
   set_name(file, "file.dat");
   if get_name(file) = "file.dat" then
      put_line(ASCII.ESC & "[92m" & "get_name(file) = ""file.dat""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set rights & get rights
   put_line("Set rights & get rights:");
   set_rights(file, (RW, R, R));
   if get_rights(file) = (RW, R, R) then
      put_line(ASCII.ESC & "[92m" & "get_rights(file) = (RW, R, R)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set size & get size
   put_line("Set size & get size:");
   set_size(file, 23568);
   if get_size(file) = 23568 then
      put_line(ASCII.ESC & "[92m" & "get_size(file) = 23568" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- set data & get data
   put_line("Set data & get data:");
   set_data(file, "thisissomeotherdata");
   if get_data(file) = "thisissomeotherdata" then
      put_line(ASCII.ESC & "[92m" & "get_data(file) = ""thisissomeotherdata""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_data(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_size(file) = 19 then
      put_line(ASCII.ESC & "[92m" & "get_size(file) = 19 (size of ""thisissomeotherdata"")" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(file) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
   -- clone
   put_line("Clone:");
   file_bis := clone(file, "/home/n7student/bin");
   if get_path(file_bis) = "/home/n7student/bin" then
      put_line(ASCII.ESC & "[92m" & "get_path(file_bis) = ""/home/n7student/bin""" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_path(file_bis) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_name(file_bis) = get_name(file) then
      put_line(ASCII.ESC & "[92m" & "get_name(file_bis) = get_name(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_name(file_bis) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_rights(file_bis) = get_rights(file) then
      put_line(ASCII.ESC & "[92m" & "get_rights(file_bis) = get_rights(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_rights(file_bis) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_size(file_bis) = get_size(file) then
      put_line(ASCII.ESC & "[92m" & "get_size(file_bis) = get_size(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_size(file_bis) is incoherent" & ASCII.ESC & "[0m");
   end if;
   if get_data(file_bis) = get_data(file) then
      put_line(ASCII.ESC & "[92m" & "get_data(file_bis) = get_data(file)" & ASCII.ESC & "[0m");
   else
      put_line(ASCII.ESC & "[91m" & "get_data(file_bis) is incoherent" & ASCII.ESC & "[0m");
   end if;
   new_line;
   
end test_file;
