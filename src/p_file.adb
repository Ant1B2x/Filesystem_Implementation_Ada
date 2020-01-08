package body P_File is

   function create (name : in String; data : in String) return T_File is
      rights : T_Rights;
      metadata : T_Metadata;
      file : T_File;
   begin
      -- rights creation
      rights := ( RW, R, R ); -- chmod 644 by default
      -- metadata creation
      metadata := create (name, rights, data'length);
      -- file creation
      file.metadata := metadata;
      file.data := data;
      
      return file;
   end create;
   
   
   function create (name : in String; rights : in T_Rights; data : in String) return T_File is
      metadata : T_Metadata;
      file : T_File;
   begin
      
      -- metadata creation
      metadata := create (name, rights, data'length);
      -- file creation
      file.metadata := metadata;
      file.data := data;
      
      return file;
   end create;
   
   function get_data (file : in T_File) return String is
   begin
      return file.data;
   end get_data;
   
   procedure set_data (file : in out T_File; data : in String) is
   begin
      file.data := data;
   end set_data;
   
   function get_metadata (file : in T_File) return T_Metadata is
   begin
      return file.metadata;
   end get_metadata;
   
   procedure set_metadata (file : in out T_File; metadata : in T_Metadata) is
   begin
      file.metadata := metadata;
   end set_metadata;
   
end P_File;
