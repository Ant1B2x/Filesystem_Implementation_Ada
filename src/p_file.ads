with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;

package P_File is

   type T_R_File is private;
   type T_File is access T_R_File;
   
   -- Role : Overload of creat with all parameters. Create a folder, and associate his name and path. 
   -- Give basic rights and empty data.
   -- Parameters :
   --    name (String) : The name of the file
   --    path (String) : The absolute path of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; path: in String) return T_File;

   -- Role : Overload of creat with all parameters. Create a folder, and associate his name, rights and path. 
   -- Give empty data.
   -- Parameters :
   --    name (String) : The name of the file
   --    rights (T_Rights) : The rights of the file
   --    path (String) : The absolute path of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; rights : in T_Rights; path: in String) return T_File;
   
   -- Role : Overload of creat with all parameters. Create a folder, and associate his name, path and data. 
   -- Give basic rights.
   -- Parameters :
   --    name (String) : The name of the file
   --    path (String) : The absolute path of the file
   --    data (String) : The data of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; path : in String; data : in String) return T_File;
   
   -- Role : Create a folder, and associate his name, rights, path and data.
   -- Parameters :
   --    name (String) : The name of the file
   --    rights (T_Rights) : The rights of the file
   --    path (String) : The absolute path of the file
   --    data (String) : The data of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; rights : in T_Rights; path : in String; data : in String) return T_File;
   
   -- Role : Return the name of the file
   -- Parameters :
   --    file (T_File) : File to get the name from
   -- Return :
   --    String : The name of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_name (file : in T_File) return String;
   
   -- Role : Set the name of the file
   -- Parameters :
   --    file (T_File) : Fle to set the name
   --    name (String) : New name to set to the file
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_name (file : in out T_File; name : in String);
   
   -- Role : Return the rights of the file, as T_Rights.
   -- Parameters :
   --    file (T_File) : File to get the rights from
   -- Return :
   --    T_Rights : The rights of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_rights (file : in T_File) return T_Rights;
   
   -- Role : The the rights of the file at given rights.
   -- Parameters :
   --    file (T_File) : The file to set the rights
   --    rights (T_Rights) : The rights to be set
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_rights (file : in out T_File; rights : in T_Rights);
   
   -- Role : Return the size of the file.
   -- Parameters :
   --    file (T_File) : File to get the size from
   -- Return :
   --    Integer : The size of the file as bytes
   -- Preconditions : /
   -- Postconditions : /
   function get_size (file : in T_File) return Integer;
   
   -- Role : Set the size of the file
   -- Parameters :
   --    file (T_File) : File to change the size from
   --    size (Integer) : The new size of the file
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_size (file : in out T_File; size : in Integer);
   
   -- Role : Return the absolute path of the file.
   -- Parameters :
   --    file (T_File) : The file to get the absolute path from
   -- Return :
   --    String : The absolute path of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_path (file : in T_File) return String;
   
   -- Role : Return the data stored in the file.
   -- Parameters :
   --    file : The file to get the data from
   -- Return :
   --    String : The data of the file, as String
   -- Preconditions : /
   -- Postconditions : /
   function get_data (file : in T_File) return String;
   
   -- Role : The the data of the file.
   -- Parameters :
   --    file (T_File) : File to set the data
   --    data (String) : Data to set to file
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_data (file : in out T_File; data : in String)
     with Pre => data'length <= SMAX_FILE;
   
   -- Role : Return a clone of the file, but as a distinct one. Need a new name and a new path
   -- Parameters :
   --    file (T_File) : File to be clone
   --    new_name (String) : New name to give to the clone
   --    new_path (String) : New path to set to the clone
   -- Return :
   --    T_File : The clone of file, with new_name as name, and new_path as path
   -- Preconditions : /
   -- Postconditions : /
   function clone (file : in T_File; new_name : in String; new_path : in String) return T_File;
   
   -- Role : Overload of clone with possibility to give new path.
   -- Create a clone of the file, but as a distinct one. Need a new path.
   -- Parameters :
   --    file (T_File) : File to be clone
   --    new_path (String) : New path to set to the clone
   -- Return :
   --    T_File : The clone of the file, with new_path as path.
   -- Preconditions : /
   -- Postconditions : /
   function clone (file : in T_File; new_path : in String) return T_File;
   
private
   type T_R_File is record
      metadata : T_Metadata;
      data : String(1..SMAX_FILE);
   end record;
   
end P_File;
