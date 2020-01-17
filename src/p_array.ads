generic
   type T is private;

package P_Array is
   
   type T_Array is private;

   NMAX_VALUES : constant Integer := 100;
   
   function create return T_Array
     with Post => get_nb_values(create'Result) = 0;
   
   function get_nb_values (f_array : in T_Array) return Integer;
   
   function get_value (f_array : in T_Array; index : in Integer) return T
     with Pre => index > 0 and index <= get_nb_values(f_array);
   
   function get_values (f_array: in T_Array; index_first: in Integer; index_last: in Integer) return T_Array
     with Pre => index_first > 0 and index_first <= index_last and index_last <= get_nb_values(f_array);
   
   procedure add_value (f_array : in out T_Array; value : in T)
     with Pre => get_nb_values(f_array) < NMAX_VALUES;
   
   procedure del_value (f_array : in out T_Array; value : in T);
      
private
   type T_Values is array (1..NMAX_VALUES) of T;
   type T_Array is record
      values : T_Values;
      nb_values : Integer;
   end record;

end P_Array;
