with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Array;

procedure test_array is
   
   package P_Array_Integer is new P_Array (T => Integer);
   use P_Array_Integer;
   
   -- print an integer array
   procedure print_array (array_integer : in T_Array) is
   begin
      put("[");
      for i in 1..get_nb_values(array_integer) loop
         put(get_value(array_integer, i), 1);
         if i /= get_nb_values(array_integer) then
            put(", ");
         end if;
      end loop;
      put("]");
      new_line;
   end print_array;
   
   array_integer : P_Array_Integer.T_Array;
begin
   -- create
   put_line("Create:");
   array_integer := create;
   print_array(array_integer);
   if get_nb_values(array_integer) = 0 then
      put_line("nb_values = 0");
   else
      put_line("nb_values is incoherent");
   end if;
   new_line;
   
   -- add value
   put_line("Add value:");
   add_value(array_integer, 10);
   print_array(array_integer);
   add_value(array_integer, 20);
   print_array(array_integer);
   add_value(array_integer, 30);
   print_array(array_integer);
   add_value(array_integer, 40);
   print_array(array_integer);
   add_value(array_integer, 50);
   print_array(array_integer);
   new_line;
   
   -- get nb values
   put_line("Get nb values:");
   if get_nb_values(array_integer) = 5 then
      put_line("nb_values = 5");
   else
      put_line("nb_values is incoherent");
   end if;
   new_line;
   
   -- del value
   put_line("Del value:");
   del_value(array_integer, 4); -- the deleted value does not exist
   print_array(array_integer);
   del_value(array_integer, 40); -- the deleted value exists
   print_array(array_integer);
   del_value(array_integer, 10); -- delete first value
   print_array(array_integer);
   del_value(array_integer, 50); -- delete last value
   print_array(array_integer);
   new_line;
   
   -- get value
   put_line("Get value:");
   if get_value(array_integer, 2) = 30 then
      put_line("array_integer[2] = 30");
   else
      put_line("array_integer[2] is incoherent");
   end if;
   new_line;
   
end test_array;
