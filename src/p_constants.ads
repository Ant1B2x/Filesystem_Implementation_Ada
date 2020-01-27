package P_Constants is
   
   -- maximum elements per array
   NMAX_VALUES : constant Integer := 100;
   -- maximum number of folders + files inside a folder
   NMAX_FOLDERS_FILES : constant Integer := 2*NMAX_VALUES;
   -- maximum string length
   LMAX_STRING : constant Integer := 100;
   -- maximum file size = 1Go
   SMAX_FILE : constant Integer := 1000*1000*1000;
   -- folder size = 10Ko
   FOLDER_SIZE : constant Integer := 10*1000;
   -- file separator, name of the root folder
   FILE_SEPARATOR : constant Character := '/';

end P_Constants;
