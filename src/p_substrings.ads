with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Array;

-- Unbounded_string https://openclassrooms.com/fr/courses/900279-apprenez-a-programmer-avec-ada/894191-les-chaines-de-caracteres
package P_Substrings is
   
   package P_Substrings_Array is new P_Array (T => Unbounded_String);   
   subtype T_Substrings is P_Substrings_Array.T_Array;
   
   function create_substrings return T_Substrings;
   
   procedure add_substring (substrings : in out T_Substrings; substring : in Unbounded_String);
   
   function split_string (original : in String; separator : in Character) return T_Substrings;
   
   function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String;
   
   function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String;
   
end P_Substrings;
