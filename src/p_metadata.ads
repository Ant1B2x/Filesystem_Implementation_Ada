with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Constants; use P_Constants;

package P_Metadata is
   
   -- raised when a name contain an invalid caracter
   Invalid_Character_Error : Exception;
   Empty_Name_Error : Exception;
   
   type E_Rights is (NONE,R,W,X,RW,RX,WX,RWX);
   type T_Rights is Array (1..3) of E_Rights;
   type T_Metadata is private;
   
   -- Role : Create the special metadata associated to the root folder.
   -- As its root, it need special name and path, that can't be set with classic function due to verifications.
   -- Parameters :
   --    /
   -- Return :
   --    T_Metadata : The special metadata attached to a root folder.
   -- Preconditions : /
   -- Postconditions : /
   function create_root return T_Metadata;
   
   -- Role : Create metadata, with associated parameters
   -- Parameters :
   --    name (String) : The name associated at the metadata
   --    rights (T_Rights) : The rights to store in the metadata
   --    size (Integer) : The size of the entity carrying this metadata
   --    path (String) : The path of the object, accessible in those metadara
   -- Return :
   --    T_Metadata : The new metadata, created in accordance with the parameters
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; rights : in T_Rights; size : in Integer; path : in String) return T_Metadata;
   
   -- Role : Return the name associated at the metadata
   -- Parameters :
   --    metadata (T_Metadata) : The metadata to get the name from
   -- Return :
   --    String : The name as String
   -- Preconditions : /
   -- Postconditions : /
   function get_name (metadata : in T_Metadata) return String;
   
   -- Role : Set the name to the metadata
   -- Parameters :
   --    metadata (T_Metadata) : The metadata to set the name
   --    name (String) : The new name to set to the metadata
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   -- raise an exception if the name contain a '/' caracter
   procedure set_name (metadata : in out T_Metadata; name : in String)
     with Pre => name'length <= LMAX_STRING;
   
   -- Role : Return the rights stored in the metadata
   -- Parameters :
   --    metadata (T_Metadata) : The metadata to get the rights from
   -- Return :
   --    T_Rights : The wanted rights.
   -- Preconditions : /
   -- Postconditions : /
   function get_rights (metadata : in T_Metadata) return T_Rights;
   
   -- Role : Set new rights to the metadata
   -- Parameters :
   --    metadata (T_Metadata) : Metadata to set the new rights
   --    rights (T_Rights) : The new rights to be set
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_rights (metadata : in out T_Metadata; rights : in T_Rights);
   
   -- Role : Return the size of the entity carrying the metadata
   -- Parameters :
   --    metadata (T_Metadata) : Metadata to get the size from
   -- Return :
   --    Integer : Size as bytes
   -- Preconditions : /
   -- Postconditions : /
   function get_size (metadata : in T_Metadata) return Integer;
   
   -- Role : Set the size to the metadata
   -- Parameters :
   --    metadata (T_Metadata) : The metadata to change the size from
   --    size (Integer) : The new size of the metadata, as bytes
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_size (metadata : in out T_Metadata; size : in Integer)
     with Pre => size <= SMAX_FILE;
   
   -- Role : Return the absolute path of the object, accessible in those metadata
   -- Parameters :
   --    metadata (T_Metadata) : Metadata to get the absolute path from
   -- Return :
   --    String : The absolute path as String.
   -- Preconditions : /
   -- Postconditions : /
   function get_path (metadata : in T_Metadata) return String;
   
   -- Role : Set the path to the metadata
   -- Parameters :
   --    metadata (T_Metadata) : Metadata to set the path
   --    path (String) : The new path to be set
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_path (metadata : in out T_Metadata; path : in String)
     with Pre => path'length <= LMAX_STRING;
      
private
   type T_Metadata is record
      name : Unbounded_String;
      rights : T_Rights;
      size : Integer;
      path : Unbounded_String;
   end record;
   
end P_Metadata;
