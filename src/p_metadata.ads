package P_Metadata is
   
   LMAX_NAME : constant Integer := 100; -- maximum name length
   SMAX_FILE : constant Integer := 10000000; -- maximum file size
   
   type E_Rights is (R,W,X,RW,RX,WX,RWX);
   type T_Rights is Array (1..3) of E_Rights;
   type T_Metadata is record
      name : String(1..LMAX_NAME);
      rights : T_Rights;
      size : Integer;
   end record;
   
   function create (name : in String; rights : in T_Rights; size : in Integer) return T_Metadata
     with Pre => size <= SMAX_FILE and name'length <= LMAX_NAME;
   
   function get_name (metadata : in T_Metadata) return String;
   
   procedure set_name (metadata : in out T_Metadata; name : in String)
     with Pre => name'length <= LMAX_NAME;
   
   function get_rights (metadata : in T_Metadata) return T_Rights;
   
   procedure set_rights (metadata : in out T_Metadata; rights : in T_Rights);
   
   function get_size (metadata : in T_Metadata) return Integer;
   
   procedure set_size (metadata : in out T_Metadata; size : in Integer)
     with Pre => size <= SMAX_FILE;
      
end P_Metadata;
