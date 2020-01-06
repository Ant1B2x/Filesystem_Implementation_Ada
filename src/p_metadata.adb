package body P_Metadata is
   
   function get_name (metadata : in T_Metadata) return String is
   begin
      return metadata.name;
   end get_name;
   
   procedure set_name (metadata : in out T_Metadata; name : in String) is
   begin
      metadata.name := name;
   end set_name;
   
   function get_rights (metadata : in T_Metadata) is
   begin
      return metadata.rights;
   end get_rights;
   
   procedure set_rights (metadata : in out T_Metadata; rights : in T_Rights) is
   begin
      metadata.rights := rights;
   end set_rights;
   
   function get_size (metadata : in T_Metadata) return Long_Integer is
   begin
      return metadata.size;
   end get_size;
   
   procedure set_size (metadata : in out T_Metadata; size : in Long_Integer) is
   begin
      metadata.size := size;
   end set_size;
      
end P_Metadata;
