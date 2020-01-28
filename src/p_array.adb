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
      -- R0 : Récupérer plusieurs valeurs et les retourner dans un tableau
      -- R1 : Trouver le nombre de valeurs à récupérer (R1.2)
      --      Récupérer les valeurs voulues(R2.2)
      
      -- R2.1 : Comment R1.2
      -- new_array.nb_values <- (index_last - index_first + 1);
      -- R2.2 : Comment R1.2
      -- new_array.values(1..get_nb_values(new_array)) <- f_array.values(index_first..index_last);
      
      
      -- set nb_values to the right number
      new_array.nb_values := (index_last - index_first + 1);
      -- add the specified values to the new array and return it
      new_array.values(1..get_nb_values(new_array)) := f_array.values(index_first..index_last);
      return new_array;
   end get_values;
   
   procedure add_value (f_array : in out T_Array; value : in T) is
   begin
      -- R0 : Ajouter une valeur
      -- R1 : Se placer sur la case suivant du tableau (R1.2)
      --      Inscrire la valeur dans la case(R2.2)
      
      -- R2.1 : Comment R1.2
      -- f_array.nb_values <- f_array.nb_values + 1;
      -- R2.2 : Comment R1.2
      -- f_array.values(f_array.nb_values) <- value;
      
      
      f_array.nb_values := f_array.nb_values + 1;
      f_array.values(f_array.nb_values) := value;
   end add_value;
   
   procedure del_value (f_array : in out T_Array; value : in T) is
      index : Integer;
   begin
      
      -- R0 : Supprimer la valeur value
      -- R1 : Trouver la valeur (R1.1)
      --      Supprimer la valeur (R2.2)
      
      -- R2.1.1 : Comment R1.1
      -- Parcourir toutes les valeurs et s'arrêter lorsque la valeur est trouvée ou qu'il n'y a plus de valeur à parcourir (R2.1.2)
      -- R2.1 : Comment R1.2
      -- Je vérifie que la valeur a été trouvé (R2.2.1)
      -- Je compacte le tableau si c'est le cas (R2.2.2)
      
      -- R3.1 : Comment R2.1.2
      --     index <- 1
      --     Tant que j'ai des valeurs à parcourir et que je n'ai as trouvé la valeur
      --         Je passe à la valeur suivante
      --     Fin tant que
      -- R3.2 : Comment R2.2.1
      --     Si index < nombre_d'éléments(f_array)
      -- R3.3 : Comment R2.2.2
      --     Je parcours toutes les valeurs depuis celle trouvée, et je place la valeur de la case suivante dans la case actuelle(R3.3.1)
      --     Je signifie que le nombre d'éléments du tableau a diminué de 1(R3.3.2)
      
      -- R4.1 : Comment R3.3.1
      --     Tant qu'il me reste des valeurs à parcourir
      --         Je mets la valeur de la case suivante dans la case courante(R4.1.1)
      --         Je passe à la case suivante(R4.1.2)
      --     Fin tant que
      -- R4.2 : Comment R3.3.2
      --     f_array.nb_values <- f_array.nb_values -1
      
      -- R5.1 : Comment R4.1.1
      --     f_array.values(index) <- f_array.values(index + 1);
      -- R5.2 : Comment R4.1.2
      --     index <- index + 1;
      
      -- find the value
      index := 1;
      while index <= get_nb_values(f_array) and then f_array.values(index) /= value loop
         index := index + 1;
      end loop;
      -- if the value is found
      if index <= get_nb_values(f_array) then
         -- compact the array
         while index < get_nb_values(f_array) loop
            f_array.values(index) := f_array.values(index + 1);
            index := index + 1;
         end loop;
         -- delete an element
         f_array.nb_values := f_array.nb_values - 1;
      end if;
   end del_value;

end p_array;
