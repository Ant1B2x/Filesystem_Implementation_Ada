with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;

package P_File is

   type T_R_File is private;
   type T_File is access T_R_File;
   
   function create (name : in String; path: in String) return T_File;

   function create (name : in String; rights : in T_Rights; path: in String) return T_File;
   
   function create (name : in String; path : in String; data : in String) return T_File;
   
   function create (name : in String; rights : in T_Rights; path : in String; data : in String) return T_File;
   
   function get_name (file : in T_File) return String;
   
   procedure set_name (file : in out T_File; name : in String);
   
   function get_rights (file : in T_File) return T_Rights;
   
   procedure set_rights (file : in out T_File; rights : in T_Rights);
   
   function get_size (file : in T_File) return Integer;
   
   function get_path (file : in T_File) return String;
   
   procedure set_path (file : in out T_File; path : in String);
   
   function get_data (file : in T_File) return String;
   
   procedure set_data (file : in out T_File; data : in String)
     with Pre => data'length <= SMAX_FILE;
   
private
   type T_R_File is record
      metadata : T_Metadata;
      data : String(1..SMAX_FILE);
   end record;
   
   -- size is only set via set_data
   procedure set_size (file : in out T_File; size : in Integer);
   
end P_File;
