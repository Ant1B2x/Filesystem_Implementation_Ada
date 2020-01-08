package body P_File is

   function create (name : in String; rights : in T_Rights; data : in String) return T_File is
      file : T_File;
   begin
      
      -- set file properties
      set_name(file, name);
      set_rights(file, rights);
      set_data(file, data);
      
      return file;
   end create;
   
   function create (name : in String; data : in String) return T_File is
      rights : T_Rights;
   begin
      
      -- rights creation
      rights := ( RW, R, R ); -- chmod 644 by default
      -- file creation
      return create (name, rights, data);
      
   end create;
   
   function get_data (file : in T_File) return String is
   begin
      return file.data;
   end get_data;
   
   procedure set_data (file : in out T_File; data : in String) is
   begin
      file.data := data;
   end set_data;
   
   function get_name (file : in T_File) return String is
   begin
      return P_Metadata.get_name(file.metadata);
   end get_name;
   
   procedure set_name (file : in out T_File; name : in String) is
   begin
      P_Metadata.set_name(file.metadata, name);
   end set_name;
   
   function get_rights (file : in T_File) return T_Rights is
   begin
      return P_Metadata.get_rights(file.metadata);
   end get_rights;
      
   procedure set_rights (file : in out T_File; rights : in T_Rights) is
   begin
      P_Metadata.set_rights(file.metadata, rights);
   end set_rights;
   
   function get_size (file : in T_File) return Integer is
   begin
      return P_Metadata.get_size(file.metadata);
   end get_size;
   
   procedure set_size (file : in out T_File; size : in Integer) is
   begin
      P_Metadata.set_size (file.metadata, size);
   end set_size;
   
   
end P_File;
