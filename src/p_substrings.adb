package body P_Substrings is

   function create_substrings return T_Substrings is
   begin
      return P_Substrings_Array.create;
   end create_substrings;

   procedure add_substring (substrings : in out T_Substrings; substring : in Unbounded_String) is
   begin
      P_Substrings_Array.add_value(substrings, substring);
   end add_substring;

   function split_string (original : in String; separator : in Character) return T_Substrings is
      substrings : T_Substrings;
      i: Integer; -- index
      substring_first : Integer;
   begin
      substrings := P_Substrings_Array.create;
      i := 1;
      substring_first := 1;
      while i <= original'Length loop
         if original(i) = separator then
            add_substring(substrings, To_Unbounded_String(original(substring_first..(i - 1))));
            while i < original'Length and then original(i + 1) = separator loop
               i := i + 1;
            end loop;
            substring_first := i + 1;
         end if;
         i := i + 1;
      end loop;
      if original(original'Last) /= separator then
         add_substring(substrings, To_Unbounded_String(original(substring_first..original'Last)));
      end if;
      return substrings;
   end split_string;

   function get_nb_substrings (substrings : in T_Substrings) return Integer is
   begin
      return P_Substrings_Array.get_nb_values(substrings);
   end get_nb_substrings;

   function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String is
   begin
      return P_Substrings_Array.get_value(substrings, index);
   end get_substring;

   function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String is
   begin
      return To_String(get_substring(substrings, index));
   end get_substring_to_string;

end P_Substrings;
