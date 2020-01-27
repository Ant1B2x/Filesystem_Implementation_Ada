package body P_File is
   
   -- private create function
   function create (name : in String; rights : in T_Rights; data : in String; path : in String) return T_File is
      file : T_File;
   begin
      -- instantiate T_R_File
      file := new T_R_File;
      -- set file properties
      file.all.metadata := P_Metadata.create(name, rights, 0, path);
      set_data(file, data);
      return file;
   end create;
   
   function create (name : in String; rights : in T_Rights := (RW, R, R); data : in String := "") return T_File is
   begin
      return create(name, rights, data, ""); -- set blank path by default
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
   
   function get_data (file : in T_File) return String is
   begin
      return file.all.data(1..get_size(file));
   end get_data;
   
   procedure set_data (file : in out T_File; data : in String) is
   begin
      file.all.data(1..data'length) := data;
      set_size(file, data'length); -- set the size to be equal to the length of the data
   end set_data;
   
   function clone (file : in T_File; new_name : in String; new_path : in String) return T_File is
      new_file : T_File;
   begin
      new_file := create(new_name, get_rights(file), get_data(file), new_path);
      return new_file;
   end clone;
   
   function clone (file : in T_File; new_path : in String) return T_File is
   begin
      return clone(file, get_name(file), new_path);
   end clone;
   
end P_File;
