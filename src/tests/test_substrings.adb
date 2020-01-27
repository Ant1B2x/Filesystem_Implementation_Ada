with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Substrings; use P_Substrings;

procedure test_substrings is
   substrings : T_Substrings;
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
   put_line("Splitting ""ab/cd/ef/gh/"" as substrings");
   substrings := split_string("ab/cd/ef/gh/", '/');
   new_line;
   
   -- get nb substrings
   put_line("Get nb substrings:");
   put_line("get_nb_substrings(substrings) = " & Integer'Image(get_nb_substrings(substrings)));
   new_line;
   
   -- get substring
   put_line("Get substring (Unbounded_String):");
   if get_substring(substrings, 2) = To_Unbounded_String("cd") then
      put_line("get_substring(substrings, 2) = To_Unbounded_String(""cd"")");
   else
      put_line("get_substring(substrings, 2) is incoherent");
   end if;
   new_line;
   
   -- get substring to string
   put_line("Get substring (String):");
   if get_substring_to_string(substrings, 2) = "cd" then
      put_line("get_substring_to_string(substrings, 2) = ""cd""");
   else
      put_line("get_substring_to_string(substrings, 2) is incoherent");
   end if;
   new_line;

   -- get substrings
   put_line("Get substrings:");
   if get_substring_to_string(get_substrings(substrings, 2, 4), 2) = "ef" then
      put_line("get_substring_to_string(get_substrings(substrings, 2, 4), 2) = ""ef""");
   else
      put_line("get_substrings(substrings, 2, 4) is incoherent");
   end if;
   new_line;
   
   -- add substring
   put_line("Add substring:");
   add_substring(substrings, "ij");
   if get_substring_to_string(substrings, 5) = "ij" then
      put_line("get_substring_to_string(substrings, 5) = ""ij""");
   else
      put_line("get_substring_to_string(substrings, 5) is incoherent");
   end if;
   new_line;
   
end test_substrings;
