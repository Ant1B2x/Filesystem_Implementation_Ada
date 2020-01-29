with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with P_Array;

package P_Substrings is

   -- These types are public because their implementations need to be known by the rest of the program
   package P_Substrings_Array is new P_Array (T => Unbounded_String);
   subtype T_Substrings is P_Substrings_Array.T_Array;

   -- Role : Create a new T_Substrings, T_Array of Unbounded_String, empty
   -- Parameters : /
   -- Return :
   --    T_Substrings : The new T_Substrings, the T_Array of Unbounded_String
   -- Preconditions : /
   -- Postconditions : /
   function create_substrings return T_Substrings;

   -- Role : Split the given String into substrings, each substring is an Unbounded_String
   -- Each time that the String contain a separator, it will create a new substring from the precedent separator
   -- The split will not put separator in substrings
   -- If several separators follow each others, they won't be put in substrings either
   -- Parameters :
   --    original (in String) : The String to split
   --    separator (in Character) : The Character used to split the string
   -- Return :
   --    T_Substrings : The splitted String, as a T_Array of Unbounded_String, each Unbounded_String repreasenting a part of the String between two separators
   -- Preconditions : /
   -- Postconditions : /
   function split_string (original : in String; separator : in Character) return T_Substrings;

   -- Role : Return the number of substrings of a T_Substrings
   -- Parameters :
   --    substrings (in T_Substring) : T_Substrings to get the number of substrings from
   -- Return :
   --    Integer : The number of substrings countained in the array
   -- Preconditions : /
   -- Postconditions : /
   function get_nb_substrings (substrings : in T_Substrings) return Integer;

   -- Role : Return a specific substring, at the given index paramater, as Unbounded_String
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    substrings (in T_Substrings) : The array of substrings
   --    index (in Integer) : The index of the wanted substring
   -- Return :
   --    Unbounded_String : The wanted substring
   -- Preconditions : /
   -- Postconditions : /
   function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String;

   -- Role : Return a specific substring, at the given index paramater, as String
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    substrings (in T_Substrings) : The array of substrings
   --    index (in Integer) : The index of the wanted substring
   -- Return :
   --    String : The wanted substring, as String
   -- Preconditions : /
   -- Postconditions : /
   function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String;

   -- Role : Return a bunch of substrings in a new T_Substrings from the given
   -- T_Substrings parameter, starting at index_first, ending at index_last
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    substrings (in T_Substrings) : The array of substrings
   --    index_first (in Integer) : The starting index of the bunch of substrings
   --    index_last (in Integer) : The ending index of the bunch substring
   -- Return :
   --    T_Substrings : The new T_Substrings countaining the wanted bunch of substrings
   -- Preconditions : /
   -- Postconditions : /
   function get_substrings (substrings: in T_Substrings; index_first: Integer; index_last: Integer) return T_Substrings;

   -- Role : Add a new substring to an existing array of substrings
   -- No preconditions because they're already in T_Array
   -- Parameters :
   --    substrings (in out T_Substrings) : The existing array of substrings to add the substring into
   --    substring (in String) : The new substring to add
   -- Return : /
   -- Preconditions : /
   -- Postconditions : /
   procedure add_substring (substrings : in out T_Substrings; substring : in String);

end P_Substrings;
