with P_Constants; use P_Constants;
with P_Metadata; use P_Metadata;

package P_File is

   -- rest of the program need to know that T_File is a pointer
   type T_R_File is private;
   type T_File is access T_R_File;
   
   -- Role : Create a folder, and associate his name, rights, and data
   -- Parameters :
   --    name (in String) : The name of the file
   --    rights (in T_Rights) := (RW, R, R) : The rights of the file
   --    data (in String) := "" : The data of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; rights : in T_Rights := (RW, R, R); data : in String := "") return T_File;
   
   -- Role : Return the name of a file
   -- Parameters :
   --    file (in T_File) : The file to get the name from
   -- Return :
   --    String : The name of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_name (file : in T_File) return String;
   
   -- Role : Set name to a file
   -- Parameters :
   --    file (in T_File) : The file to set the name
   --    name (in String) : The new name of the file
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_name (file : in out T_File; name : in String);
   
   -- Role : Return the rights of a file
   -- Parameters :
   --    file (in T_File) : The file to get the rights from
   -- Return :
   --    T_Rights : The rights of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_rights (file : in T_File) return T_Rights;
   
   -- Role : Set rights to a file
   -- Parameters :
   --    file (in out T_File) : The file to set the rights
   --    rights (in T_Rights) : The new rights of the file
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_rights (file : in out T_File; rights : in T_Rights);
   
   -- Role : Return the size of a file
   -- Parameters :
   --    file (in T_File) : The file to get the size from
   -- Return :
   --    Integer : The size of the file as bytes
   -- Preconditions : /
   -- Postconditions : /
   function get_size (file : in T_File) return Integer;
   
   -- Role : Set size to a file
   -- Parameters :
   --    file (in out T_File) : The file to set the size
   --    size (in Integer) : The new size of the file
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure set_size (file : in out T_File; size : in Integer);
   
   -- Role : Return the absolute path of a file
   -- Parameters :
   --    file (in T_File) : The file to get the absolute path from
   -- Return :
   --    String : The absolute path of the file
   -- Preconditions : /
   -- Postconditions : /
   function get_path (file : in T_File) return String;
   
   -- Role : Return the data stored in a file
   -- Parameters :
   --    file : The file to get the data from
   -- Return :
   --    String : The data of the file, as String
   -- Preconditions : /
   -- Postconditions : /
   function get_data (file : in T_File) return String;
   
   -- Role : Set data to a file
   -- Parameters :
   --    file (in out T_File) : The file to set the data
   --    data (in String) : The new data of the file
   -- Return : /
   -- Preconditions :
   --    the length of data is inferior or equal to the maximum size of a file (because 1 character = 1 byte)
   -- Postconditions : /
   procedure set_data (file : in out T_File; data : in String)
     with Pre => data'length <= SMAX_FILE;
   
   -- Role : Return a clone of the file, but as a distinct one, need a new name and a new path
   -- Parameters :
   --    file (in T_File) : File to clone
   --    new_name (in String) : New name to give to the clone
   --    new_path (in String) : New path to set to the clone
   -- Return :
   --    T_File : The clone of file, with new_name as name, and new_path as path
   -- Preconditions : /
   -- Postconditions : /
   function clone (file : in T_File; new_name : in String; new_path : in String) return T_File;
   
   -- Role : Overload of clone with possibility to only give new path (and keep the current name)
   -- Return a clone of the file but as a distinct one, need a new path
   -- Parameters :
   --    file (in T_File) : File to clone
   --    new_path (in String) : New path to set to the clone
   -- Return :
   --    T_File : The clone of the file, with new_path as path
   -- Preconditions : /
   -- Postconditions : /
   function clone (file : in T_File; new_path : in String) return T_File;
   
private
   -- Here, we are assuming that 1 character = 1 byte
   type T_R_File is record
      metadata : T_Metadata;
      data : String(1..SMAX_FILE);
   end record;
   
   -- Role : Create a folder, and associate his name, rights, data and path
   -- Parameters :
   --    name (String) : The name of the file
   --    rights (T_Rights) : The rights of the file
   --    data (String) : The data of the file
   --    path (String) : The absolute path of the file
   -- Return :
   --    T_File : The new created file
   -- Preconditions : /
   -- Postconditions : /
   function create (name : in String; rights : in T_Rights; data : in String; path : in String) return T_File;
   
end P_File;
