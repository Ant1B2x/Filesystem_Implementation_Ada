package body p_array is

   function create return T_Array is
      f_array : T_Array;
   begin
      f_array.nb_values := 0;
      return f_array;
   end create;
   
   function get_nb_values (f_array : in T_Array) return Integer is
   begin
      return f_array.nb_values;
   end get_nb_values;
   
   function get_value (f_array : in T_Array; index : in Integer) return T is
   begin
      return f_array.values(index);
   end get_value;
   
   function get_values(f_array: T_Array; index_first: Integer; index_last: Integer)return T_Array is
      new_array : T_Array;
   begin
      -- Set nb_values to the right number
      new_array.nb_values := (index_last - index_first + 1);
      -- Add the specified values to the new array
      new_array.values(1..get_nb_values(new_array)) := f_array.values(index_first..index_last);
      return new_array;
   end get_values;
   
   procedure add_value (f_array : in out T_Array; value : in T) is
   begin
      f_array.nb_values := f_array.nb_values + 1;
      f_array.values(f_array.nb_values) := value;
   end add_value;
   
   procedure del_value (f_array : in out T_Array; value : in T) is
      index : Integer;
   begin
      -- look for the value
      index := 1;
      while index <= get_nb_values(f_array)
        and then f_array.values(index) /= value loop
         index := index + 1;
      end loop;
      -- if the sibling is found
      if index <= get_nb_values(f_array) then
         -- compact
         while index < get_nb_values(f_array) loop
            f_array.values(index) := f_array.values(index + 1);
            index := index + 1;
         end loop;
         -- delete an element
         f_array.nb_values := f_array.nb_values - 1;
      end if;
   end del_value;

end p_array;
