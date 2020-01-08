with P_Metadata; use P_Metadata;

package p_file is

   type T_File is private;
   
   function create (name : in String; data : in String) return T_File
     with Pre => data'length <= SMAX_FILE and name'length <= LMAX_NAME;
   
   function create (name : in String; rights : in T_Rights; data : in String) return T_File
     with Pre => data'length <= SMAX_FILE and name'length <= LMAX_NAME;
   
   function get_data (file : in T_File) return String;
   
   procedure set_data (file : in out T_File; data : in String)
     with Pre => data'length <= SMAX_FILE;
   
   function get_metadata (file : in T_File) return T_Metadata;
   
   procedure set_metadata (file : in out T_File; metadata : in T_Metadata);
   
private
   type T_File is record
      metadata : T_Metadata;
      data : String(1..SMAX_FILE);
   end record;

end p_file;
