with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with P_Substrings; use P_Substrings;

procedure test_substrings is
   substrings : T_Substrings;
   char : Character;
begin
   -- create substrings
   put_line("Create an empty array of substrings:");
   substrings := create_substrings;
   if get_nb_substrings(substrings) = 0 then
      put_line("get_nb_substrings(substrings) = 0");
   else
      put_line("get_nb_substrings(substrings) is incoherent");
   end if;
   new_line;
   
   -- split string
   put_line("Split string:");
   put_line("Splitting ""ab cd ef gh"" as substrings");
   substrings := split_string(" . ", '.');
   
   -- get nb substrings
   put_line("Get nb substrings:");
   put(get_nb_substrings(substrings));
   Put_Line("Hahaha");
   Put_Line(get_substring_to_string(substrings, 1));
   
   

   --function split_string (original : in String; separator : in String) return T_Substrings;

   --function get_nb_substrings (substrings : in T_Substrings) return Integer;

   --function get_substring (substrings : in T_Substrings; index : in Integer) return Unbounded_String;

   --function get_substring_to_string (substrings : in T_Substrings; index : in Integer) return String;

   --function get_substrings(substrings: in T_Substrings; index_first: Integer; index_last: Integer) return T_Substrings;
end test_substrings;
