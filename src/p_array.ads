with P_Constants; use P_Constants;

generic
   type T is private;

package P_Array is
   
   type T_Array is private;
   
   -- Role : Create a new T_Array
   -- Parameters :
   --    /
   -- Return :
   --    T_Array : The new array as T_Array
   -- Preconditions : /
   -- Postconditions : with Post => get_nb_values(create'Result) = 0;
   function create return T_Array
     with Post => get_nb_values(create'Result) = 0;
   
   -- Role : Return the number of entity stored in the array
   -- Parameters :
   --    f_array (T_Array) : The array to get the number of values from
   -- Return :
   --    Integer : The number of entity stored in f_array
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_values (f_array : in T_Array) return Integer;
   
   -- Role : Get the entity at the position index. Entity are sorted by insertion
   -- Parameters :
   --    f_array (T_Array) : Array to get the entity from
   --    index (Integer) : Index of the wanted entity
   -- Return :
   --    T : The wanted entity
   -- Preconditions : /
   -- Postconditions : /
   function get_value (f_array : in T_Array; index : in Integer) return T
     with Pre => index > 0 and index <= get_nb_values(f_array);
   
   -- Role : Return a new T_Array containing the wanted values.
   -- The bunch of values starts at index_first and stops at index_last of the originale array (f_array).
   -- Parameters :
   --    f_array (T_Array) : T_Array to get the values from
   --    index_first (Integer) : First value to get the values from
   --    index_last (Integer) : Last value to get the values from
   -- Return :
   --    T_Array : The new array with the wanted values
   -- Preconditions : index_first > 0 and index_first <= index_last and index_last <= get_nb_values(f_array)
   -- Postconditions : T_Array.nb_values = index_last - index_first + 1
   function get_values (f_array: in T_Array; index_first: in Integer; index_last: in Integer) return T_Array
     with Pre => index_first > 0 and index_first <= index_last and index_last <= get_nb_values(f_array),
     Post => T_Array.nb_values = index_last - index_first + 1;
   
   -- Role : Add an entity to the f_array
   -- Parameters :
   --    f_array (T_Array) : T_Array to add the entity
   --    value (T) : New entity to add
   -- Return :
   --    /
   -- Preconditions : get_nb_values(f_array) < NMAX_VALUES
   -- Postconditions : /
   procedure add_value (f_array : in out T_Array; value : in T)
     with Pre => get_nb_values(f_array) < NMAX_VALUES;
   
   -- Role : Delete an entity in the f_array
   -- Parameters :
   --    f_array (T_Array) : T_Array to del the entity from
   --    value (T) : The entity to delete from the T_Array
   -- Return :
   --    /
   -- Preconditions : /
   -- Postconditions : /
   procedure del_value (f_array : in out T_Array; value : in T);
      
private
   type T_Values is array (1..NMAX_VALUES) of T;
   type T_Array is record
      values : T_Values;
      nb_values : Integer;
   end record;

end P_Array;
