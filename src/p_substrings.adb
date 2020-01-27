package body P_Substrings is

   function create_substrings return T_Substrings is
   begin
      return P_Substrings_Array.create;
   end create_substrings;

   function split_string (original : in String; separator : in Character) return T_Substrings is
      substrings : T_Substrings; -- returned T_Substrings
      index : Integer; -- index of the different substrings
      index_substring_first : Integer; -- index of the beggining of a substring
   begin
      substrings := P_Substrings_Array.create;
      -- if the original string is blank, return an empty array of substrings
      if original = "" then
         return substrings;
      end if;

      index := 1;
      index_substring_first := 1;
      -- for all the original string
      while index <= original'Length loop
         -- if the current character is the separator
         if original(index) = separator then
            -- check if we really need to add string, because original(1..0) will be add, and we don't want this
            if index_substring_first <= (index - 1) then
               -- add the substring
               add_substring(substrings, original(index_substring_first..(index - 1)));
            end if;
            -- while the separator is encountered, skip it
            while index < original'Length and then original(index + 1) = separator loop
               index := index + 1;
            end loop;
            index_substring_first := index + 1;
         end if;
         index := index + 1;
      end loop;
      -- if the last character is not a separator
      if original(original'Last) /= separator then
         -- add the last substring
         add_substring(substrings, original(index_substring_first..original'Last));
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

   function get_substrings(substrings: in T_Substrings; index_first: Integer; index_last: Integer) return T_Substrings is
      new_substrings: T_Substrings; -- T_Substrings to return
   begin
      -- get the substrings between index_first and index_last
      new_substrings := P_Substrings_Array.get_values(substrings, index_first, index_last);
      return new_substrings;
   end get_substrings;

   procedure add_substring (substrings : in out T_Substrings; substring : in string) is
   begin
      P_Substrings_Array.add_value(substrings, To_Unbounded_String(substring));
   end add_substring;

end P_Substrings;
