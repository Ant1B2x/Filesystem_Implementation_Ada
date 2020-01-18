with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Array;

package P_Substrings is

   package P_Substrings_Array is new P_Array (T => Unbounded_String);
   subtype T_Substrings is P_Substrings_Array.T_Array;

   -- create an empty array of substrings
   function create_substrings return T_Substrings;

   function split_string (original : in String; separator : in Character) return T_Substrings;

   function get_nb_substrings (substrings : in T_Substrings) return Integer;

   function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String;

   function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String;

   function get_substrings(substrings: in T_Substrings; index_first: Integer; index_last: Integer) return T_Substrings;

private
   procedure add_substring (substrings : in out T_Substrings; substring : in String);

end P_Substrings;
