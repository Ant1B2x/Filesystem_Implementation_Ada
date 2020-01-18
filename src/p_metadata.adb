package body P_Metadata is
   
   function create_root return T_Metadata is
      metadata : T_Metadata;
   begin
      metadata.name := To_Unbounded_String(""&FILE_SEPARATOR);
      set_rights(metadata, (RWX, RX, RX));
      set_size(metadata, FOLDER_SIZE);
      set_path(metadata, "");
      return metadata;
   end create_root;
   
   function create (name : in String; rights : in T_Rights; size : in Integer; path : in String) return T_Metadata is
      metadata : T_Metadata;
   begin
      set_name(metadata, name);
      set_rights(metadata, rights);
      set_size(metadata, size);
      set_path(metadata, path);
      return metadata;
   end create;
   
   function get_name (metadata : in T_Metadata) return String is
   begin
      return To_String(metadata.name);
   end get_name;
   
   procedure set_name (metadata : in out T_Metadata; name : in String) is
   begin
      
      if name'length = 0 then
         raise Empty_Name_Error;
      end if;
      
      -- a name can't contain a file separator or ' '
      for i in 1..name'length loop
         if name(i) = FILE_SEPARATOR or name(i) = ' ' then
            raise Invalid_Character_Error;
         end if;
      end loop;
      
      metadata.name := To_Unbounded_String(name);
   end set_name;
   
   function get_rights (metadata : in T_Metadata) return T_Rights is
   begin
      return metadata.rights;
   end get_rights;
   
   procedure set_rights (metadata : in out T_Metadata; rights : in T_Rights) is
   begin
      metadata.rights := rights;
   end set_rights;
   
   function get_size (metadata : in T_Metadata) return Integer is
   begin
      return metadata.size;
   end get_size;
   
   procedure set_size (metadata : in out T_Metadata; size : in Integer) is
   begin
      metadata.size := size;
   end set_size;
   
   function get_path (metadata : in T_Metadata) return String is
   begin
      return To_String(metadata.path);
   end get_path;
     
   procedure set_path (metadata : in out T_Metadata; path : in String) is
   begin
      metadata.path := To_Unbounded_String(path);
   end set_path;
      
end P_Metadata;
