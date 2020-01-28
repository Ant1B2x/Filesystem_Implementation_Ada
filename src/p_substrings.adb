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
      -- R0 : Separer la chaine de caracteres en fonction d'un separateur, et retourner les valeurs dans un tableau de chain de caracteres
      -- R1 : Si la chaine est vide(R1.1)
      --      Si la chaine n'est pas vide(R1.2)
      -- R2.1 : Comment R1.1
      --      Je retourne un tableau de chaine de caracteres vide
      -- R2.2 : Comment R1.2
      --      Tant que je parcours tous les charactere de la chaine jusqu'au dernier Faire(R2.2.1)
      --          Si le caractere courant est un separateur(R2.2.2)
      --             J'ajoute tous les caractres parcourus depuis le dernier separateur jusqu'au caractere precedent (en tant que chaine de caractere), dans le tableau a retourner(R2.2.3)
      --             Je compte le nombre de separateur consecutif, et je me déplace jusqu'a ce que ce ne soit plus un separateur (R2.2.4)
      --             Je mets à jour la position du dernier separateur rencontre (R2.2.5)
      --          Fin Si
      --      Fin tant que
      --      Si le dernier caractere n'est pas un separateur, j'ajoute la derniere chaine au tableau(R2.2.6)
      --      Retourner le tableau de sous-chaines(R2.2.7)

      -- R3.1 : Comment R2.1.1
      --      index <- 1
      --      index_charactere_apres_dernier_separateur <- 1
      --      Tant que index < longueur(original) Faire
      --          ...
      --          ...
      --          index <- index +1
      --      Fin tant que
      -- R3.2 : Comment R2.1.2
      --      Si original(index) = separator
      -- R3.3 : Comment R2.1.3
      --      sous_chaines <- Creer_tableau_chaines
      --      ajouter_chaine(sous_chaines, original(index_charactere_apres_dernier_separateur.. index - 1))
      -- R3.4 : Comment R2.1.4
      --      Tant que je peux parcourir des caracteres et que le caractere suivant est un separateur Faire(R3.4.1)
      --          Je me deplace au caractere suivant(R3.4.2)
      --      Fin tant que
      -- R3.5 : Comment R2.1.5
      --      index_charactere_apres_dernier_separateur <- index + 1;
      -- R3.6 : Comment R2.1.6
      --      Si original(longueur(original)) /= separator Alors
      --          ajouter_chaine(sous_chaines, original(index_charactere_apres_dernier_separateur.. longueur(original)))
      --      Fin si
      -- R3.7 : Comment R2.1.7
      --      Retourn sous_chaines

      -- R4.1 : Comment R3.4.1
      --      index < longueur(original) et ensuite original(index +1) = separator
      -- R4.2 : Comment R3.4.2
      --      index <- index +1


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
