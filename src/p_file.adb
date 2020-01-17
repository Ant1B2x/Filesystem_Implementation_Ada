package body P_File is
   
   function create (name : in String; path: in String) return T_File is
   begin
      -- file creation
      return create (name, path, "");
   end create;

   function create (name : in String; rights : in T_Rights; path: in String) return T_File is
   begin
      -- file creation
      return create (name, rights, path, "");
   end create;

   function create (name : in String; path : in String; data : in String) return T_File is
      rights : T_Rights;
   begin
      
      -- rights creation
      rights := ( RW, R, R ); -- chmod 644 by default
      -- file creation
      return create (name, rights, path, data);
      
   end create;
   
   function create (name : in String; rights : in T_Rights; path : in String; data : in String) return T_File is
      file : T_File;
   begin
      
      -- instantiate T_R_File
      file := new T_R_File;
      -- set file properties
      set_name(file, name);
      set_rights(file, rights);
      set_path(file, path);
      set_data(file, data);
      
      return file;
   end create;
   
   function get_name (file : in T_File) return String is
   begin
      return P_Metadata.get_name(file.all.metadata);
   end get_name;
   
   procedure set_name (file : in out T_File; name : in String) is
   begin
      P_Metadata.set_name(file.all.metadata, name);
   end set_name;
   
   function get_rights (file : in T_File) return T_Rights is
   begin
      return P_Metadata.get_rights(file.all.metadata);
   end get_rights;
      
   procedure set_rights (file : in out T_File; rights : in T_Rights) is
   begin
      P_Metadata.set_rights(file.all.metadata, rights);
   end set_rights;
   
   function get_size (file : in T_File) return Integer is
   begin
      return P_Metadata.get_size(file.all.metadata);
   end get_size;
   
   procedure set_size (file : in out T_File; size : in Integer) is
   begin
      P_Metadata.set_size (file.all.metadata, size);
   end set_size;
   
   function get_path (file : in T_File) return String is
   begin
      return P_Metadata.get_path(file.all.metadata);
   end get_path;
   
   procedure set_path (file : in out T_File; path : in String) is
   begin
      P_Metadata.set_path (file.all.metadata, path);
   end set_path;
   
   function get_data (file : in T_File) return String is
   begin
      return file.all.data(1..get_size(file));
   end get_data;
   
   procedure set_data (file : in out T_File; data : in String) is
   begin
      file.all.data(1..data'length) := data;
      set_size(file, data'length);
   end set_data;
   
end P_File;
