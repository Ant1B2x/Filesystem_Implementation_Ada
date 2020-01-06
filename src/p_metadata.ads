package P_Metadata is
   
   SMAX_NAME : constant Integer := 100;
   
   type T_Rights is private;
   type T_Metadata is record
      name : String(1..SMAX_NAME);
      rights : T_Rights;
      size : Long_Integer;
   end record;
   
   function get_name (metadata : in T_Metadata) return String;
   
   procedure set_name (metadata : in out T_Metadata; name : in String)
     with Pre => name'length <= SMAX_NAME;
   
   function get_rights (metadata : in T_Metadata) return T_Rights;
   
   procedure set_rights (metadata : in out T_Metadata; rights : in T_Rights);
   
   function get_size (metadata : in T_Metadata) return Long_Integer;
   
   procedure set_size (metadata : in out T_Metadata; size : in Long_Integer);
   
private
   
   type T_Rights is (R,W,X,RW,RX,WX,RWX);       
      
end P_Metadata;
