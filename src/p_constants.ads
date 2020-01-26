package P_Constants is
   
   -- maximum elements per array
   NMAX_VALUES : constant Integer := 100;
   -- maximum number of folders + files inside a folder
   NMAX_FOLDERS_FILES : constant Integer := 2*NMAX_VALUES;
   -- maximum string length
   LMAX_STRING : constant Integer := 100;
   -- maximum file size
   SMAX_FILE : constant Integer := 1000000000; -- 1Go
   -- folder size
   FOLDER_SIZE : constant Integer := 10000; -- 10Ko
   -- file separator
   FILE_SEPARATOR : constant Character := '/';

end P_Constants;
