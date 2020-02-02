package body P_Commands is
   
   -- #################################################### PUBLIC SUBROUTINES ############################################################
   
   function command_to_string (encoded_command : in E_Encoded_Commands) return String is
   begin
      return E_Encoded_Commands'Image(encoded_command);
   end command_to_string;
   
   procedure run_command(current_directory : in out T_Folder; command_line : in String) is
      splitted_line : T_Substrings; -- splitted command line
      nb_arguments : Integer; -- number of arguments in the command line (counting the called command)
      
      options : T_Substrings := create_substrings; -- options to pass to the called command
      parameters : T_Substrings := create_substrings; -- parameters to pass to the called command
   begin
      -- split the command line
      splitted_line := split_string(command_line, ' ');
      nb_arguments := get_nb_substrings(splitted_line);

      -- if we have more than one argument, that means we have options or parameters
      if nb_arguments > 1 then
         -- decode arguments from 2 to N in the options and parameters variables
         get_options_and_parameters(get_substrings(splitted_line, 2, nb_arguments), options, parameters);
      end if;

      -- execute the called command
      case E_Encoded_Commands'Value(get_substring_to_string(splitted_line, 1)) is
         when ls => ls_command(current_directory, options, parameters);
         when rm => rm_command(current_directory, options, parameters);
         when pwd => pwd_command(current_directory, options, parameters);
         when cd => cd_command(current_directory, options, parameters);
         when mkdir => mkdir_command(current_directory, options, parameters);
         when cp => cp_command(current_directory, options, parameters);
         when mv => mv_command(current_directory, options, parameters);
         when touch => touch_command(current_directory, options, parameters);
         when tar => tar_command(current_directory, options, parameters);
         when help => help_command(options, parameters);
         when clear => clear_command;
      end case;
   exception
      when Invalid_Character_Error =>
         put_line("A path you entered contain an invalid character.");
      when Invalid_Folder_Error =>
         put_line("A specified path is incorrect, no such directory.");
      when Invalid_File_Error =>
         put_line("A specified path is incorrect, no such file.");
      when Copy_Into_Itself_Error =>
         put_line("Cannot copy a directory into itself.");
      When Same_Name_Error =>
         put_line("Cannot create file or directory: A file or directory with same name already exists.");
      when Full_Folder_Error =>
         put_line("Cannot add file or directory: The directory is already full.");
      when Empty_Option_Error =>
         put_line("Empty option.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      when Not_Handled_Option_Error =>
         put_line("Not handled option.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      when Wrong_Parameters_Number_Error =>
         put_line("Wrong number of parameters.");
         put_line("Try help '" & get_substring_to_string(splitted_line, 1) & "' for more information.");
      when Constraint_Error =>
         put_line("command not found");
         put_line("Try 'help' to see a list of available commands.");
   end run_command;
   
   -- ############################################ VARIOUS PRIVATE SUBROUTINES ###########################################################
   
   procedure get_options_and_parameters (arguments : in T_Substrings; options : in out T_Substrings; parameters : in out T_Substrings) is
   begin
      -- R0 : Separer les options des parametres, dans les arguments donnes en entree
      -- R1 : Parcourir les arguments(R1.1)
      --      Separer les options(R1.2)
      --      Separer les parametres(R1.3)
      --
      -- R2.1 : Pour tous les arguments Faire(R2.1.1)
      --          ...
      --          ...
      --          ...
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si c'est à mettre dans options Alors(R2.1.2)
      --          Si il n'y a pas d'option Alors(R2.1.3)
      --              Erreur Option_Vide_Erreur
      --          Fin si
      --          Ajouter aux options(R2.1.4)
      --      Fin si
      -- R2.3 : Comment R1.3
      --      Sinon
      --          Ajouter aux parametres(R2.1.5)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Pour index allant de 1 à nombre(arguments)
      -- R3.2 : Comment R2.1.2
      --      Si argument(arguments, index)(1) = '-' Alors
      -- R3.3 : Comment R2.1.3
      --      Si longueur(argument(arguments, index)) < 2
      -- R3.4 : Comment R2.1.4
      --      Pour tous les caracteres dans l'options en cours Faire(R3.4.2)
      --          Ajouter option dans options(R3.4.2)
      --      Fin pour
      -- R3.5 : Comment R2.1.5
      --      ajouter(parameters, argument(arguments, index))
      
      -- R4.1 : Comment R3.4.1
      --      Pour tous les caracteres allant de 1 a longueur(argument(arguments, index)) Faire  
      -- R4.2 : Comment R3.4.2
      --      ajouter(options, argument(arguments, index))
      
      
      -- For every argument
      for i in 1..get_nb_substrings(arguments) loop
         -- If it starts with a "-", then it's an option
         if get_substring_to_string(arguments, i)(1) = '-' then
            -- If we only have "-", we raise an error
            if get_substring_to_string(arguments, i)'Length < 2 then
               raise Empty_Option_Error;
            end if;
            -- We start at 2 because we need to get all options, and not the "-" character
            for j in 2..get_substring_to_string(arguments, i)'Length loop
               -- We have to transform it into a string, because the (j) part return a Character, and we add it to the options array
               add_substring(options, get_substring_to_string(arguments, i)(j) & "");
            end loop;
         else
            -- Else it's an argument and we add it to the arguments array
            add_substring(parameters, get_substring_to_string(arguments, i));
         end if;
      end loop;
   end get_options_and_parameters;
   
   function split_options (options_as_string : in String) return T_Substrings is
      splitted_option : T_Substrings; -- splitted options following "-" separator (like ["-ab"], ["-cd"])
      options : T_Substrings; -- all options (like ["a","b","c","d"])
   begin
      -- R0 : Recuprer les differentes options depuis une chaine de caractere
      -- R1 : Recuperer les options par paquets(R1.1)
      --      Parcourir les paquets d'options(R1.2)
      --      Separer les options(R1.3)
      --
      -- R2.1 : Comment R1.1
      --      paquets_options <- separer_avec_separateur(options, '-')
      -- R2.2 : Comment R1.2
      --      Pour toutes les paquets_options Faire(R2.2.1)
      --          ...
      --          ...
      --          ...
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si le paquet contient plusieurs options(R2.3.1)
      --          Pour toutes les options du paquet Faire(R2.3.2)
      --              Ajouter aux options(R2.3.3)
      --          Fin pour
      --      Sinon
      --          Ajouter le paquet aux option(R2.3.4)
      --      Fin si

      
      -- R3.1 : Comment R2.2.1
      --      Pour index_paquet allant de 1 à nombre(paquets_options)
      -- R3.2 : Comment R2.3.1
      --      Si longueur(argument(paquets_options, index_paquet)) > 1' Alors
      -- R3.3 : Comment R2.3.2
      --      Pour index_option allant de 1 à longueur(argument(paquets_options, index_paquet)) Faire
      -- R3.4 : Comment R2.3.3
      --          ajouter(options, arugument(paquets_options, index_paquet)(index_option))
      -- R3.5 : Comment R2.3.4
      --      ajouter(options, arugument(paquets_options, index_paquet)(1))
      
      
      options := create_substrings;
      splitted_option := split_string(options_as_string, '-');
      for i in 1.. get_nb_substrings(splitted_option) loop
         -- If there is multiple option wrote after a "-" (like "-rf")
         if get_substring_to_string(splitted_option, i)'Length > 1 then
            -- We add each one of them to the options
            for j in 1.. get_substring_to_string(splitted_option, i)'Length loop
               add_substring(options, ""&get_substring_to_string(splitted_option, i)(j));
            end loop;
         -- Else, It's a simple option like "-r"
         else
            -- we add the option to the options
            add_substring(options, ""&get_substring_to_string(splitted_option, i)(1));
         end if;
      end loop;
      return options;
   end split_options;
   
   function contains_option (options : in T_Substrings; option : in Character) return Boolean is
   begin
      -- R0 : Verifiers si le tableau contenant les options contient celle passee en parametre
      -- R1 : Parcourir toutes les options(R1.1)
      --      Verifier si l'option recherchee y est(R1.2)
      --      Sinon retourne faux(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Pour toutes les options Faire(R2.1.1)
      --          ...
      --          ...
      --          ...
      --      Fin pour
      
      -- R2.2 : Comment R1.2
      --      Si c'est l'option recherchee Alors(R2.2.1)
      --          Retourne Vrai
      --      Fin si
      -- R2.3 : Comment R1.3
      --      Retourne Faux
      
      -- R3.1 : Comment R2.1.2
      --      Pour index allant de 1 à nombre(options)
      -- R3.2 : Comment R2.2.1
      --      Si argument(options, index)(1) = option Alors
      
      
      
      for i in 1..get_nb_substrings(options) loop
         -- return True when the desired option is found
         if get_substring_to_string(options, i)(1) = option then
            return True;
         end if;
      end loop;
      -- return False anyway
      return False;
   end contains_option;
   
   function only_handled_options (options : in T_Substrings; handled_options : in String) return Boolean is
      splitted_handled_options : T_Substrings;
   begin
      -- R0 : Verifiers si  les valeurs dans options sont comprises dans handled_options
      -- R1 : Reruperer les differentes options comprises dans handled_options(R1.1)
      --      Parcourir toutes les options(R1.2)
      --      Verifier si l'option n'appartient pas a handled_options(R1.3)
      --      Retourne faux si elle n'appartient pas a handled_options(R1.4)
      --      Retourner vrai a la fin, si toutes les options sont comprises dans handled_options(R1.5)
      
      -- R2.1 : Comment R1.1
      --      handled_options_separes <- handled_options(options)
      -- R2.2 : Comment R1.2
      --      Pour toutes les options Faire(R2.1.1)
      --          ...
      --          ...
      --          ...
      --      Fin pour
      
      -- R2.3 : Comment R1.3
      --      Si l'option n'appartient pas à handled_options_separes Alors(R2.2.1)
      --          ....
      --      Fin si
      -- R2.4 : Comment R1.4
      --      Retourne Faux
      -- R2.5 : Comment R1.5
      --      Retourne Vrai
      -- R2.6 : Comment R1.6
      --      
      
      -- R3.1 : Comment R2.1.2
      --      Pour index allant de 1 a nombre(options)
      -- R3.2 : Comment R2.2.1
      --      Si non countient_option(handled_options_separes, argument(options, index)(1)) Alors
      
      
      splitted_handled_options := split_options(handled_options);
      for i in 1..get_nb_substrings(options) loop
         -- return False if a provided option is not supported
         if not contains_option(splitted_handled_options, get_substring_to_string(options, i)(1)) then
            return False;
         end if;
      end loop;
      -- return True anyway
      return True;
   end only_handled_options;
   
   function go_to_folder (original_directory : in T_Folder; path : in String; stop_at_penultimate : in Boolean := False) return T_Folder is
      current: T_Folder; -- current directory
      path_tree: T_Substrings; -- directory tree
      penultimate: Integer; -- 1 if stop_at_penultimate, 0 else
   begin
      -- R0 : Aller jusqu'au dossier voulu, à partir du dossier courrant et du chemin
      -- R1 : Si le chemin donné n'est pas vide(R1.1)
      --      Si le chemin commence par '/', on part dossier originel(R1.2)
      --      Sinon, on par du dossier courant(R1.3)
      --      On verifir si on doit s'arreter a l'avant derniere entite du chemin(R1.4)
      --      On separe le chemin en les differents noms de dossier qu'il represente(R1.5)
      --      On parcours tous les noms de dossiers contenus dans le chemin(R1.6)
      --      Si le nom ne correspond a aucun enfant du dossier courant, on leve une exception(R1.7)
      --      Sinon, on passe au descendant suivant(R1.8)
      --      On retourne le dossier de fin de parcours(R1.9)
      
      -- R2.1 : Comment R1.1
      --      Si longueur(path) <= 0 Alors
      --          Erreur Dossier_Invalide_Erreur
      --      Fin si
      -- R2.2 : Comment R1.2
      --      Si path(1) = '/' Alors
      --          On part du dossier originel(R2.2.1)
      --      Fin si
      -- R2.3 : Comment R1.3
      --      Si path(1) /= '/' Alors
      --          On part du dossier courant(R2.3.1)
      --      Fin si
      -- R2.4 : Comment R1.4
      --      Si stop_at_penultimate Alors
      --          avant_dernier <- 1
      --      Sinon
      --          avant_dernier <- 0
      --      Fin si
      -- R2.5 : Comment R1.5
      --      arbre_descendants <- separer_avec_separateur(path, '/')
      -- R2.6 : Comment R1.6
      --      Pour index allant de longueur(arbre_descendants) - avant_dernier Faire
      --          ...
      --          ...
      --          ...
      --      Fin pour
      -- R2.7 : Comment R1.7
      --      Si est_null(dossier enfant recherche) Alors(R2.7.1)
      --          Erreur Dossier_Invalide_Erreur
      --      Fin si
      -- R2.8 : Comment R1.8
      --      courant <- dossier_fils(dossier enfant recherche)(R2.8.1)
      -- R2.9 : Comment R1.9
      --      Retourne courant
      --      
      
      -- R3.1 : Comment R2.2.2
      --      courant = dossier_originel()(R2.2.1)
      -- R3.2 : Comment R2.3.1
      --      courant = folderR(2.3.1)
      -- R3.3 : Comment R2.7.2
      --      Si est_null(dossier_fils(courant, argument(arbre_descendants, index))) Alors
      -- R3.4 : Comment R2.8.1
      --      courant <- dossier_fils(courant, argument(arbre_descendants, index))
      
      
      -- we can't go to a blank path
      if path'Length <= 0 then
         raise Invalid_Folder_Error;
      end if;
      
      -- if this is an absolute path
      if path(path'First) = FILE_SEPARATOR then
         -- start from root
         current := get_root;
      else
         -- else, start from the current folder
         current := original_directory;            
      end if;
      
      -- we might want to stop at the penultimate of the given path, when using mkdir for example
      penultimate := (if stop_at_penultimate then 1 else 0);
      -- follow the path from the defined start
      path_tree := split_string(path, FILE_SEPARATOR);
      for i in 1..get_nb_substrings(path_tree) - penultimate loop
         -- if the following directory doesn't exist, the path is invalid, raise an exception
         if is_null(find_folder(current, get_substring_to_string(path_tree, i))) then
            raise Invalid_Folder_Error;
         end if;
         -- take the next sibling as current
         current := find_folder(current, get_substring_to_string(path_tree, i));
      end loop;
      return current;
   end go_to_folder;
   
   -- Here we choose to use Unbounded_String because we don't know the length of the return
   function get_name_from_path (path : in Unbounded_String) return Unbounded_String is
      nb_substrings : Integer; -- represents how many folders are separated by "/" in the path
   begin
      -- R0 : Recupere le nom de la derniere entite depuis un chemin
      -- R1 : Separe les differentes entites(R1.1)
      --      Recupere le nombre d'entites(R1.2)
      --      Si le nombre d'entite est nule, retourne une valeur speciale(R1.3)
      --      Retourne la derniere entite des entites separees(R1.4)
      
      -- R2.1 : Comment R1.1
      --      entites <- separer_avec_separateur(path, '/')
      
      -- R2.2 : Comment R1.2
      --      nombre_entites <- longueur(entites)
      -- R2.3 : Comment R1.3
      --      Si nombre_entites = 0 Alors
      --          Retourne '/'
      --      Fin si
      -- R2.3 : Comment R1.3
      --      Retourne argument(entites, nombre_entites)
      
      nb_substrings := get_nb_substrings(split_string(To_String(path), FILE_SEPARATOR));
      -- if only "/" is provided in path, return "/"
      if nb_substrings = 0 then
         return To_Unbounded_String(""&FILE_SEPARATOR);
      end if;
      return get_substring(split_string(To_String(path), FILE_SEPARATOR), nb_substrings);
   end get_name_from_path;
   
   function calculate_size (current_directory : in T_Folder) return Integer is
      current_folder_size: Integer; -- folder size, incremented at each file or directory
   begin
      -- R0 : Fonction recursive retournant la somme des tailles de tous les fichiers et sous-dossier non directs et directs d'un dossier, plus la taille de ce même dossier
      -- R1 : Ajoute la taille du dossier actuel(R1.1)
      --      Ajoute la taille de tous les fichiers(R1.2)
      --      Ajoute la taille de calculate_size de chaque sous-dossier(R1.3)
      --      Retourner la taille complete(R1.4)
      
      -- R2.1 : Comment R1.1
      --      taille_actuelle <- FOLDER_SIZE;
      -- R2.2 : Comment R1.2
      --      Pour toutes les fichiers du dossier actuel Faire(R2.2.1)
      --          Ajouter la taille du fichier(R2.2.2)
      --      Fin pour
      
      -- R2.3 : Comment R1.3
      --      Pour toutes les dossiers du dossier actuel Faire(R2.3.1)
      --          Ajouter la taille de calculate_size du dossier(R2.3.2)
      --      Fin pour
      --      
      -- R2.4 : Comment R1.4
      --      Retourne taille_actuelle
      
      -- R3.1 : Comment R2.2.1
      --      Pour index_fichier allant de 1 à nombre_fichiers(current_directory) Faire
      -- R3.2 : Comment R2.2.2
      --       taille_actuelle <- taille_actuelle + taille(fichier(current_directory, index_fichier))
      -- R3.3 : Comment R2.3.1
      --      Pour index_dossier allant de 1 à nombre_dossiers(current_directory) Faire
      -- R3.4 : Comment R2.3.2
      --      taille_actuelle <- taille_actuelle + calculate_size(dossier(current_directory, index_dossier))
      
      
      -- folder size is 10Ko by default
      current_folder_size := FOLDER_SIZE;
      -- + files size
      for i in 1..get_nb_files(current_directory) loop
         current_folder_size := current_folder_size + get_size(get_file(current_directory, i));
      end loop;
      -- + subfolders size
      for i in 1..get_nb_folders(current_directory) loop
         current_folder_size := current_folder_size + calculate_size(get_folder(current_directory, i));
      end loop;
      return current_folder_size;
   end calculate_size;
   
   function compare_T_R_Siblings (sibling1, sibling2 : in T_R_Sibling) return Boolean is
   begin
      -- return true if the name of the first sibling is inferior to the name of the second
      if sibling1.name <= sibling2.name then
         return True;
      -- else, return false
      else
         return False;
      end if;
   end compare_T_R_Siblings;
   
   function create_siblings_set (current_directory : in T_Folder) return T_Siblings_Set is
      siblings_set : T_Siblings_Set; -- returned sibling set
      new_element : T_R_Sibling; -- new element of the set
   begin
      -- R0 : Prend un dossier, et renvoie un ensemble tries contenant tous les noms de ses fichiers et dossiers, en gardant en mémoire si le nom appartient à un dossier ou à un fichier
      -- R1 : Enregistre tous les noms de fichiers de current_directory dans l'ensemble(R1.1)
      --      Enregistre tous les noms de sous-dossier directs de current_directory dans l'ensemble(R1.2)
      --      Retourne l'ensemble(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Pour tous les fichiers de current_directory Faire(R2.1.1)
      --          Enregistrer son nom en tant que fichier(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Pour tous les sous-dossiers directs de current_directory Faire(R2.2.1)
      --          Enregistrer son nom en tant que fichier(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Retourne ensemble_noms_descendants_tries
      
      -- R3.1 : Comment R2.1.1
      --      Pour index_fichier allant de 1 à nombre_fichiers(current_directory) Faire
      -- R3.2 : Comment R2.1.2
      --       nouveau_nom.nom <- nom(fichier(current_directory, index_fichier))
      --       nouveau_nom.est_nom_dossier <- Faux
      --       ensemble_noms_descendants_tries.inserer(nouveau_nom)
      -- R3.3 : Comment R2.2.1
      --       Pour index_dossier allant de 1 à nombre_dossiers(current_directory) Faire
      -- R3.4 : Comment R2.2.2
      --       nouveau_nom.nom <- nom(dossier(current_directory, index_dossier))
      --       nouveau_nom.est_nom_dossier <- Vrai
      --       ensemble_noms_descendants_tries.inserer(nouveau_nom)
      
      -- add each folder
      for i in 1.. get_nb_folders(current_directory) loop
         new_element.name := To_Unbounded_String(get_name(get_folder(current_directory, i)));
         new_element.is_folder := True;
         siblings_set.insert(new_element);
      end loop;
      -- add each file
      for i in 1.. get_nb_files(current_directory) loop
         new_element.name := To_Unbounded_String(get_name(get_file(current_directory, i)));
         new_element.is_folder := False;
         siblings_set.insert(new_element);
      end loop;
      return siblings_set;
   end create_siblings_set;
   
   procedure display_folders_and_files (siblings_set : in T_Siblings_Set) is
   begin
      -- R0 : Affiche tous les noms contenus dans un ensemble de noms, en affichant les noms des dossiers en couleur
      -- R1 : Pour tous les noms contenus dans l'ensemble(R1.1)
      --      Si le nom appartient à un dossier, l'affiche en couleur(R1.2)
      --      Sinon, l'affiche normalement(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Pour nom_descendant dans ensemble_noms_descendant_tries Faire(R2.1.1)
      --          ...
      --          ...
      --          ...
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si nom_descendant.est_nom_dossier Alors
      --          Afficher nom en couleur(R2.2.1)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si non nom_descendant.est_nom_dossier Alors
      --          Afficher nom normalement(R2.3.1)
      --      Fin pour
      
      -- R3.1 : Comment R2.2.1
      --      Ecrire(Caractere_ASCII_Echapement + "[95m")
      --      Ecrire(nom(nom_descendant) + "    ")
      --      Ecrire(Caractere_ASCII_Echapement + "[0m")
      -- R3.1 : Comment R2.3.1
      --      Ecrire(nom(nom_descendant) + "    ")
      
      for sibling of siblings_set loop
         -- if this is a folder, color it
         if sibling.is_folder then
            put(ASCII.ESC & "[95m");
            put(To_String(sibling.name) & "  ");
            put(ASCII.ESC & "[0m");
         else
            put(To_String(sibling.name) & "  ");
         end if;
      end loop;
      new_line;
   end display_folders_and_files;
   
   procedure folder_deep_copy (source_folder : in T_Folder; destination_folder : in out T_Folder) is
      original_file : T_File; -- original file
      new_file : T_File; -- copy of original file
      original_folder : T_Folder; -- original folder
      new_folder : T_Folder; -- copy of a folder
   begin
      -- R0 : Fonction recursive copiant le contenu d'un dossier dans l'autre, et faisant de même pour tous les sous-dossier directs
      -- R1 : Copie tous les fichiers de source_folder dans destination_folder(R1.1)
      --      Copie tous les dossiers source_folder dans destination_folder et se rappelle pour chaque sous-dossiers de source_folder(R1.2)
      
      -- R2.1 : Comment R1.1
      --      Pour toutes les fichiers de source_folder actuel Faire(R2.1.1)
      --          Copier le fichier dans destination_folder(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Pour toutes les sous-dossiers directs de source_folder actuel Faire(R2.2.1)
      --          Copier le sous-dossier direct dans destination_folder(R2.2.2)
      --          rappeler folder_deep_copy du sous-dossier actuel de source_folder et du sous-dossier venant d'etre cree dans destination_folder(R2.2.3)
      --      Fin pour
      
      -- R3.1 : Comment R2.1.1
      --      Pour index_fichier allant de 1 à nombre_fichiers(current_directory) Faire
      -- R3.2 : Comment R2.1.2
      --       fichier_original <- fichier(source_folder, index_fichier)
      --       nouveau_fichier <- clone_fichier(fichier_original, pwd(destination_folder))
      --       ajouter_fichier(destination_folder, nouveau_fichier)
      -- R3.3 : Comment R2.2.1
      --      Pour index_dossier allant de 1 à nombre_dossiers(current_directory) Faire
      -- R3.4 : Comment R2.2.2
      --      dossier_original <- dossier(source_folder, index_dossier)
      --      nouveau_fichier <- creer_dossier(nom(dossier_original), destination_folder, droits(dossier_original))
      -- R3.5 : Comment R2.2.3
      --      folder_deep_copy(dossier(source_folder, index_dossier), nouveau_fichier);
      
      
      
      -- copy all files from original folder
      for i in 1..get_nb_files(source_folder) loop
         original_file := get_file(source_folder, i);
         new_file := clone(original_file, get_pwd(destination_folder));
         add_file(destination_folder, new_file);
      end loop;
      -- copy all folders from original folder
      for i in 1..get_nb_folders(source_folder) loop
         original_folder := get_folder(source_folder, i);
         new_folder := create(get_name(original_folder), destination_folder, get_rights(original_folder));
         folder_deep_copy(get_folder(source_folder, i), new_folder);
      end loop;
   end folder_deep_copy;
   
   procedure print_global_help is
   begin
      put_line("Available commands:");
      put_line("pwd      print name of current/working directory");
      put_line("cd       change the working directory");
      put_line("ls       list directory contents");
      put_line("mkdir    make directories");
      put_line("touch    create empty file");
      put_line("cp       copy files and directories");
      put_line("mv       move (rename) files");
      put_line("rm       remove files or directories");
      put_line("tar      archive a directory");
      put_line("clear    clear the terminal screen");
      put_line("help     show this menu");
      put_line("exit     cause the shell to exit");
      new_line;
      put_line("You can get more help by specifying a command.");
      put_line("For example, try 'help ls'.");
   end print_global_help;

   procedure print_specific_help (command : in String) is
   begin
      begin
         case E_Encoded_Commands'Value(command) is
            when pwd =>
               put_line("NAME");
               put_line("  pwd - print name of current/working directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  pwd");
            when cd =>
               put_line("NAME");
               put_line("  cd - change the working directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  cd DIRECTORY");
            when ls =>
               put_line("NAME");
               put_line("  ls - list directory contents");
               new_line;
               put_line("SYNOPSIS");
               put_line("  ls [-r] DIRECTORY");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      list recursively");
            when mkdir =>
               put_line("NAME");
               put_line("  mkdir - make directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  mkdir DIRECTORY");
            when touch =>
               put_line("NAME");
               put_line("  touch - create empty file");
               new_line;
               put_line("SYNOPSIS");
               put_line("  touch FILE");
            when cp =>
               put_line("NAME");
               put_line("  cp - copy files and directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  cp [-r] SOURCE DEST");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      copy a directory, copy file if omitted");
            when mv =>
               put_line("NAME");
               put_line("  mv - move (rename) files");
               new_line;
               put_line("SYNOPSIS");
               put_line("  mv [-r] SOURCE DEST");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      move a directory, move file if omitted");
            when rm =>
               put_line("NAME");
               put_line("  rm - remove files or directories");
               new_line;
               put_line("SYNOPSIS");
               put_line("  rm [-r] FILE");
               new_line;
               put_line("OPTIONS");
               put_line("  -r");
               put_line("      remove a directory, remove file if omitted");
            when tar =>
               put_line("NAME");
               put_line("  tar - archive a directory");
               new_line;
               put_line("SYNOPSIS");
               put_line("  tar FILE");
            when clear =>
               put_line("NAME");
               put_line("  clear - clear the terminal screen");
               new_line;
               put_line("SYNOPSIS");
               put_line("  clear");
            when help =>
               print_global_help;
         end case;
      exception
         when Constraint_Error =>
            put_line("No help entry for this command.");
            new_line;
            print_global_help;
      end;
   end print_specific_help;
   
   -- ################################################ PRIVATE COMMANDS ##################################################################
   
   procedure pwd_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      -- R0 : Ecrie le pwd d'un dossier
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Affiche le pwd(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Ecrit(pwd(current_directory))
      
      -- R3.1 : Comment R2.1.1
      --      Si longueur(options) /= 0 Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 0 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      
      
      
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 0 then
         raise Wrong_Parameters_Number_Error;
      end if;
      put_line(get_pwd(current_directory));
   end pwd_command;
   
   procedure cd_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
   begin
      -- R0 : Ecrie le pwd d'un dossier
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Fait suivre à current_directory le chemin contenu dans parameters(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si le chemin n'est pas vide Alors(R2.3.1)
      --          current_directory (R2.3.2)
      --      Sinon
      --          current_directory devient le dossier originel(R2.3.3)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Si longueur(options) /= 0 Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) > 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.3.1
      --      Si longueur(argument(parameters, 1)) > 0 Alors
      -- R3.6 : Comment R2.3.2
      --      current_directory <- aller_au_dossier(current_directory, argument(parameters, 1))
      -- R3.7 : Comment R2.3.3
      --      current_directory <- dossier_originel
      
      
      
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a path is specified, cd to this path
      if get_substring_to_string(parameters, 1)'Length > 0 then
         current_directory := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      else
      -- if no path is specified, cd to root directory
         current_directory := get_root;
      end if;
   end cd_command;
   
   procedure ls_command (current_directory : in T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      current : T_Folder; -- represents the folder where we are doing the "ls" in
   begin
      -- R0 : Affiche tous les fichiers et dossiers directs d'un dossier, ou parcours le fait en parcourrant tous ses sous-dossiers 
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Si j'ai un chemin comme parametre, alors je me deplace le long de ce chemin(R1.3)
      --      Si il faut afficher tous le sous arbre, alors j'affiche le dossier courrant et je propage aux sous-dossier directs(R1.4)
      --      Sinon, j'affiche le dossier courrant(R1.5)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si le chemin precise n'est pas vide Alors(R2.3.1)
      --          Je me deplace le long du chemin(R2.3.2)
      --      Fin si
      -- R2.4 : Comment R1.4
      --      Si l'option recursive est demandee Alors(R2.4.1)
      --          Je recupere le nom du dossier courant(R2.4.2)
      --          J'affiche le nom(R2.4.3)
      --          J'affiche le contenu de courant(R2.4.4)
      --          Je propage à tous ses sous-dossiers(R2.4.5)
      --      Fin si
      -- R2.5 : Comment R1.5
      --      Sinon
      --          J'affiche le contenu de courant(R2.5.1)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Si contient_seulement_option_supportees(options, "-r") Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) > 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.3.1
      --      Si longueur(argument(parametres, 1)) > 0
      -- R3.6 : Comment R2.3.2
      --      courant <- aller_au_dossier(current_directory, argument(parameters, 1));
      -- R3.7 : Comment R2.4.1
      --      Si contient_option(options, "-r") Alors
      -- R3.8 : Comment R2.4.2
      --      nom <- Si est_dossier_originel(courant) Alors "." Sinon nom(courant) Fin si
      -- R3.9 : Comment R2.4.3
      --      Ecrire(nom + ":")
      -- R3.10 : Comment R2.4.4
      --      afficher_contenu(courant)
      -- R3.11 : Comment R2.4.5
      --      Pour tous les sous-dossiers directs de courant Faire(R3.11.1)
      --          Afficher recursivement le contenu(R3.11.2)
      --      Fin pour
      -- R3.12 : Comment R2.5.1
      --      afficher_contenu(courant)
      
      -- R4.1 : Comment R3.11.1
      --      Pour index allant de 1 a nombre_dossier(courant) Faire
      -- R4.2 : Comment R3.11.2
      --      afficher_contenu_recursivement(courant, nom)
      
      
      
      -- ls only supports "-r" option
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a path is provided, do ls in this path
      if get_substring_to_string(parameters, 1)'Length > 0 then
         current := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      -- else, ls in the current directory
      else
         current := current_directory;
      end if;
      -- if options contain r, do a recursive ls on each subfolder
      if contains_option(options, 'r') then
         put_line(get_name(current) & ":");
         display_folders_and_files(create_siblings_set(current));
         for i in 1.. get_nb_folders(current) loop
            ls_r_command(get_folder(current, i), To_Unbounded_String(get_name(current)));
         end loop;
      -- else, do a standard ls
      else
         display_folders_and_files(create_siblings_set(current));
      end if;
   end ls_command;
   
   procedure ls_r_command (current_directory : in T_Folder; preceding_path : in Unbounded_String) is
      current_path : Unbounded_String; -- represents the current relative path from the folder we are doing the "ls -r" in
   begin
      -- R0 : Fonction recursive qui affiche le nom de tous les fichiers et sous-dossier directs, et se propage dans les sous-dossiers directs.
      --      Se propage depuis un dossier, en affichant le chemin relatif depuis de dossier à chaque propagation
      -- R1 : Determine le chemin relatif courant et l'affiche(R1.1)
      --      Affiche les fichiers et sous-dossiers direct de current_directory (R1.2)
      --      Se propage dans tous les sous-dossiers directs de current_directory(R1.3)
      
      -- R2.1 : Comment R1.1
      --      Recupere le chemin relatif courrant fournit par preceding_path et ajoute le nom du dossier courant(R2.1.1)
      --      Affiche ce chemin relatif(R2.1.2)
      -- R2.2 : Comment R1.2
      --      ensemble_descendants <- creer_ensemble_descendants(current_directory)
      --      afficher_dossier_directs_et_fichiers(ensemble_descendants)
      -- R2.3 : Comment R1.3
      --      Pour tous les sous-dossier directs Faire(R2.3.1)
      --          ls_r_command(sous-dossier, chemin relatif)(R2.3.1)
      --      Fin pour
      
      -- R3.1 : Comment R2.1.1
      --      chemin_relatif <- preceding + '/' + nom(current_directory)
      -- R3.2 : Comment R2.1.2
      --      Ecrire(chemin_relatif + ':')
      -- R3.3 : Comment R2.3.1
      --      Pour index_dossier allant de 1 à nombre_dossiers(current_directory) Faire
      -- R3.4 : Comment R2.3.2
      --      ls_r_command(dossier(dossier_original, index_dossier),chemin_relatif)

      
      -- check if the current directory is a root subfolder
      if not is_null(get_parent(current_directory)) and then is_root(get_parent(current_directory)) then
         -- if true, only add the subfolder name (avoiding double "/")
         current_path := preceding_path & get_name(current_directory);
      -- if this is a standard folder
      else
         -- add current path + "/" + current directory name
         current_path := preceding_path & FILE_SEPARATOR & get_name(current_directory);
      end if;
      -- print current path and ":"
      new_line;
      put_line(To_String(current_path) & ":");
      -- print current directory content
      display_folders_and_files(create_siblings_set(current_directory));
      for i in 1.. get_nb_folders(current_directory) loop
         ls_r_command(get_folder(current_directory,i), current_path);
      end loop;
   end ls_r_command;
   
   procedure mkdir_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      new_directory : T_Folder; -- the directory we are creating
      new_directory_name : Unbounded_String; -- the name of the directory we are creating
      parent : T_Folder; -- the parent of the new directory
   begin
      -- R0 : Cree un dossier avec le nom donne, depuis le chemin donne
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Je me deplace le long du chemin(R1.3)
      --      Je recupere le nom du dossier a creer(R1.4)
      --      Je cree le dossier(R1.5)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      parent <- aller_au_dossier(current_directory, argument(parameters, 1), True)
      -- R2.4 : Comment R1.4
      --      nom <- nom_depuis_chemin(argument(parameters, 1))
      -- R2.5 : Comment R1.5
      --      creer_dossier(nom, parent)
      
      -- R3.1 : Comment R2.1.1
      --      Si longueur(options) /= 0 Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      
      
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- go to the parent of the new directory
      parent := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      -- create the new directory with the given name
      new_directory_name := get_name_from_path(get_substring(parameters, 1));
      new_directory := create(To_String(new_directory_name), parent);
   end mkdir_command;
   
   procedure touch_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings)is
      new_file : T_File; -- the file we are creating
      new_file_name: Unbounded_String; -- the name of the file we are creating
      parent : T_Folder; -- the parent of the new file
   begin
      -- R0 : Cree un fichier avec le nom donne, depuis le chemin donne
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Je me deplace le long du chemin et recupere le parent du fichier a creer(R1.3)
      --      Je recupere le nom du fichier a creer(R1.4)
      --      Je cree le fichier(R1.5)
      --      J'indique au parent qu'il a un nouveau fichier(R1.6)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      parent <- aller_au_dossier(current_directory, argument(parameters, 1), True)
      -- R2.4 : Comment R1.4
      --      nom <- nom_depuis_chemin(argument(parameters, 1))
      -- R2.5 : Comment R1.5
      --      nouveau_fichier <- creer_fichier(nom)
      -- R2.6 : Comment R1.6
      --      ajouter_fichier(parent, nouveau_fichier)
      
      -- R3.1 : Comment R2.1.1
      --      Si longueur(options) /= 0 Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      
      
      
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- go to the parent of the new file
      parent := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
      -- create the new file with the given name
      new_file_name := get_name_from_path(get_substring(parameters, 1));
      new_file := create(To_String(new_file_name));
      add_file(parent, new_file);
   end touch_command;
   
   procedure cp_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      new_name : Unbounded_String; -- the name of the new file / folder
      destination_folder_parent : T_Folder; -- the parent of the destination file / folder
      source_folder : T_Folder; -- the source folder from where we are copying / we copy
      destination_folder : T_Folder; -- the destination folder in which we copy
      original_file : T_File; -- original file before copy
      original_file_name : Unbounded_String; -- original file name
      new_file : T_File; -- new file, copy of original file
   begin
      -- R0 : Copie un fichier dans un repertoir, en specifiant le nouveau nom, ou fait de meme avec un dossier et tous ses composants 
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Recupere le dossier parent de l'element a copier(R1.3)
      --      Recupere le nom de l'element a copier(R1.4)
      --      Si il faut copier un dossier, le fait(R1.5)
      --      Si il faut copier un fichier, le fait(R1.6)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      parent_dossier_destination := aller_au_dossier(current_directory, argument(parameters, 2), Vrai);
      -- R2.4 : Comment R1.4
      --      nouveau_nom := nom_depuis_chemin(argument(parameters, 2));
      -- R2.5 : Comment R1.5
      --      Si les options contiennent l'option recursive Alors(R2.5.1)
      --          Je recupere le dossier à copier(R2.5.2)
      --          Je verifie que je ne copie pas ce dossier dans un de ses parents, sinon je leve une exception(R2.5.3)
      --          Je cree le nouveau dossier qui va accueillir la copie(R2.5.4)
      --          Je place le dossier courrant dans ce nouveau dossier cree(R2.5.5)
      --          Je copie recursivement le dossier a copier dans ce nouveau repertoire(R2.5.6)
      --      Fin si
      -- R2.6 : Comment R1.6
      --      Si les options ne contiennent pas l'option recursive Alors(R2.6.1)
      --          Je recupere le dossier contenant le fichier a copier(R2.6.2)
      --          Je recupere le fichier(R2.6.3)
      --          Si le fichier n'existe pas, je leve une exception(R2.6.4)
      --          Je clone le fichier dans un nouveau(R2.6.5)
      --          J'ajoute le nouveau fichier clone dans le dossier de destination(R2.6.6)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Si contient_seulement_option_supportees(options, "-r") Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 2 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.5.1
      --      Si contient_option(options, "-r") Alors
      -- R3.6 : Comment R2.5.2
      --      dossier_source <- aller_a_dossier(current_directory, argument(parameters, 2))
      -- R3.7 : Comment R2.5.3
      --      Si a_comme_parent(parent_dossier_destination,dossier_source) Alors
      --          Erreur Copie_Dans_Un_Descendant_Erreur
      --      Fin si
      -- R3.8 : Comment R2.5.4
      --      creer_dossier(current_directory, tableau_option_vide, arguments(parameters, 2, 2))
      -- R3.9 : Comment R2.5.5
      --      dossier_destination <- aller_a_dossier(parent_dossier_destination, nouveau_nom)
      -- R3.10 : Comment R2.5.6
      --      copie_profonde_dossier(dossier_source, dossier_destination)
      -- R3.11 : Comment R2.6.1
      --      Si non contient_option(options, "-r") Alors
      -- R3.12 : Comment R2.6.2
      --      dossier_source <- aller_a_dossier(current_directory, argument(parameters, 2), Vrai)
      -- R3.13 : Comment R2.6.3
      --      Je recupere le nom du fichier original(R3.13.1)
      --      Je recupere le fichier depuis le dossier parent, grace au nom(R3.13.2)
      -- R3.14 : Comment R2.6.4
      --      Si fichier_original est null Alors
      --          Erreur Fichier_Invalide_Erreur
      --      Fin si
      -- R3.15 : Comment R2.6.5
      --      nouveau_fichier <- clone_fichier(fichier_original, nouveau_nom, pwd(parent_dossier_destination))
      -- R3.16 : Comment R2.6.6
      --      ajouter_fichier(parent_dossier_destination, nouveau_fichier)
      
      -- R4.1 : Comment R3.13.1
      --      nom_fichier_original <- nom_depuis_chemin(argument(parameters, 1))
      -- R4.2 : Comment R3.13.2
      --      fichier_original <- fichier(parent_dossier_destination, nom_fichier_original)
      
      
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 2 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- folder to put the copy in
      destination_folder_parent := go_to_folder(current_directory, get_substring_to_string(parameters, 2), True);
      -- name of the new file / folder
      new_name := get_name_from_path(get_substring(parameters, 2));
      -- if this is a recursive cp
      if contains_option(options, 'r') then
         -- folder to copy
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
         -- we can't copy a folder from itself to itself
         if has_as_parent(destination_folder_parent, source_folder) then
            raise Copy_Into_Itself_Error;
         end if;
         -- create the new folder
         mkdir_command(current_directory, create_substrings, get_substrings(parameters, 2, 2));
         -- get the new folder pointer
         destination_folder := find_folder(destination_folder_parent, To_String(new_name));
         -- starting to copy the contents from source to new
         folder_deep_copy(source_folder, destination_folder);
      else
         -- folder to get the original file from
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
         -- get the original file
         original_file_name := get_name_from_path(get_substring(parameters, 1));
         original_file := find_file(source_folder, To_String(original_file_name));
         -- if the original file is null, it means it is not valid, raise an exception
         if original_file = null then
            raise Invalid_File_Error;
         end if;
         -- create the new file as a copy of original file
         new_file := clone(original_file, To_String(new_name), get_pwd(destination_folder_parent));
         -- add this new file to the destination folder, parent of the new file
         add_file(destination_folder_parent, new_file);
      end if;
   end cp_command;
   
   procedure mv_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      source_folder : T_Folder; -- the source folder containing the file
      destination_folder : T_Folder; -- the parent of the new file
      original_file_name : Unbounded_String; -- the original file name
      new_file_name : Unbounded_String; -- the new name of the file
      original_file : T_File; -- the original file
      new_file : T_File; -- the new file
   begin
      -- R0 : Deplace un fichier dans un repertoir, en specifiant le nouveau nom, ou fait de meme avec un dossier et tous ses composants 
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Si il faut deplacer un dossier, le fait(R1.3)
      --      Si il faut deplacer un fichier, le fait(R1.4)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si les options contiennent l'option recursive Alors(R2.3.1)
      --          Je copie le dossier a deplacer dans la destination(R2.3.2)
      --          Je supprime le dossier a deplacer(R2.3.3)
      --      Fin si
      -- R2.4 : Comment R1.4
      --      Si les options ne contiennent pas l'option recursive Alors(R2.4.1)
      --          Je recupere le dossier parent du fichier a deplacer(R2.4.2)
      --          Je recupere le dossier dans lequel deplacer le fichier(R2.4.3)
      --          Je recupere le nom du fichier a deplacer(R2.4.4)
      --          Je recupere le nouveau nom du fichier un fois deplace(R2.4.5)
      --          Je recupere le fichier original(R2.4.6)
      --          Si le fichier n'existe pas, je leve une exception(R2.4.7)
      --          Je clone le fichier dans un nouveau(R2.4.8)
      --          J'ajoute le nouveau fichier clone dans le dossier de destination(R2.4.9)
      --          Je supprime le fichier a deplacer, l'original(R2.4.10)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Si contient_seulement_option_supportees(options, "-r") Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 2 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.3.1
      --      Si contient_option(options, "-r") Alors
      -- R3.6 : Comment R2.3.2
      --      cp_command(current_directory, options, parameters);
      -- R3.7 : Comment R2.3.3
      --      rm_command (current_directory, options, get_substrings(parameters, 1, 1));
      -- R3.8 : Comment R2.4.1
      --      Si non contient_option(options, "-r") Alors
      -- R3.9 : Comment R2.4.2
      --      dossier_source <- aller_a_dossier(current_directory, argument(parameters, 2), Vrai)
      -- R3.10 : Comment R2.4.3
      --      dossier_destination <- aller_a_dossier(parent_dossier_destination, nouveau_nom, Vrai)
      -- R3.11 : Comment R2.4.4
      --      nom_fichier_original <- nom_depuis_chemin(argument(parameters, 1))
      -- R3.12 : Comment R2.4.5
      --      nouveau_nom_fichier <- nom_depuis_chemin(argument(parameters, 2))
      -- R3.13 : Comment R2.4.6
      --      fichier_original <- fichier(parent_dossier_destination, nom_fichier_original)
      -- R3.14 : Comment R2.4.7
      --      Si fichier_original est null Alors
      --          Erreur Fichier_Invalide_Erreur
      --      Fin si
      -- R3.15 : Comment R2.4.8
      --      nouveau_fichier <- clone_fichier(fichier_original, nouveau_nom, pwd(parent_dossier_destination))
      -- R3.16 : Comment R2.4.9
      --      ajouter_fichier(parent_dossier_destination, nouveau_fichier)
      -- R3.17 : Comment R2.4.10
      --      supprimer_fichier(dossier_source, nom_fichier_original)
      
      
      
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 2 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if this is a recursive move
      if contains_option(options, 'r') then
         -- execute a copy
         cp_command(current_directory, options, parameters);
         -- remove the old directory
         rm_command (current_directory, options, get_substrings(parameters, 1, 1));
      else
         -- go to source and destination folders from parameters
         source_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
         destination_folder := go_to_folder(current_directory, get_substring_to_string(parameters, 2), True);
         -- get original and new file names from parameters
         original_file_name := get_name_from_path(get_substring(parameters, 1));
         new_file_name := get_name_from_path(get_substring(parameters, 2));
         -- get the original file
         original_file := find_file(source_folder, To_String(original_file_name));
         -- if the original file is null, it means it is not valid, raise an exception
         if original_file = null then
            raise Invalid_File_Error;
         end if;
         -- clone the original file as a new file
         new_file := clone(original_file, To_String(new_file_name), get_pwd(destination_folder));
         -- add the new file to the destination folder
         add_file(destination_folder, new_file);
         -- delete the original file from the source folder
         del_file(source_folder, To_String(original_file_name));
      end if;
   end mv_command;
   
   procedure rm_command (current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      parent_folder_deleted : T_Folder; -- parent folder of the deleted file / folder
      deleted_name : Unbounded_String; -- name of the deleted file / folder
   begin
      -- R0 : Supprime un fichier ou un dossier
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Si il faut supprimer un dossier, le fait(R1.3)
      --      Si il faut supprimer un fichier, le fait(R1.4)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      nom <- nom_depuis_chemin(argument(parameters, 1))
      -- R2.4 : Comment R1.4
      --      parent <- aller_au_dossier(current_directory, argument(parameters, 1), True)
      -- R2.3 : Comment R1.3
      --      Si les options contiennent l'option recursive Alors(R2.3.1)
      --          Je recupere le parent de l'element a supprimer(R2.3.2)
      --          Je recupere le nom de l'element a supprimer(R1.3.3)
      --          Si le dossier a supprimer est un parent du dossier courant, je me deplace au parent du dossier a supprimer(R2.3.4)
      --          Je supprime le dossier a supprimer(R2.3.5)
      --      Fin si
      -- R2.4 : Comment R1.4
      --      Si les options ne contiennent pas l'option recursive Alors(R2.4.1)
      --          Je recupere le parent de l'element a supprimer(R1.4.2)
      --          Je recupere le nom de l'element a supprimer(R1.4.3)
      --          Si le fichier n'est pas un fichier du parent, leve une exception(R2.4.4)
      --          Supprime le fichier(R2.4.5)
      --      Fin si
      
      -- R3.1 : Comment R2.1.1
      --      Si contient_seulement_option_supportees(options, "-r") Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.3.1
      --      Si contient_option(options, "-r") Alors
      -- R3.6 : Comment R2.3.2
      --      parent_element_a_supprimer <- aller_a_dossier(aller_a_dossier(current_directory, argument(parameters, 1)), "../")
      -- R3.7 : Comment R2.3.3
      --      nom_element_a_supprimer <- nom(aller_a_dossier(current_directory, argument(parameters, 1)))
      -- R3.8 : Comment R2.3.4
      --      Si a_comme_parent(current_directory, aller_a_dossier(current_directory, argument(parameters, 1))) Alors
      --            current_directory <- parent_element_a_supprimer
      --      Fin si
      -- R3.9 : Comment R2.3.5
      --      supprimer_dossier(parent_element_a_supprimer, nom_element_a_supprimer)
      -- R3.10 : Comment R2.4.1
      --      Si non contient_option(options, "-r") Alors
      -- R3.11 : Comment R2.4.2
      --      parent_element_a_supprimer <- aller_a_dossier(current_directory, argument(parameters, 1), Vrai)
      -- R3.12 : Comment R2.4.3
      --      nom_element_a_supprimer <- nom_depuis_chemin(argument(parameters, 1))
      -- R3.13 : Comment R2.4.4
      --      Si trouver_fichier(parent, nom) = null Alors
      --            Erreur Fichier_Invalide_Erreur
      --      Fin si
      -- R3.14 : Comment R2.4.5
      --      suprimer_fichier(parent_element_a_supprimer, nom_element_a_supprimer)

      
      
      
      if not only_handled_options(options, "-r") then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) /= 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if this is a recursive rm
      if contains_option(options, 'r') then
         -- get deleted folder parent from parameters
         parent_folder_deleted := go_to_folder(go_to_folder(current_directory, get_substring_to_string(parameters, 1)), "../");
         -- get deleted folder name from parameters 
         deleted_name := To_Unbounded_String(get_name(go_to_folder(current_directory, get_substring_to_string(parameters, 1))));
         -- if the current directory is equal to the deleted directory, go to the directory parent
         if has_as_parent(current_directory, go_to_folder(current_directory, get_substring_to_string(parameters, 1))) then
            current_directory := parent_folder_deleted;
         end if;
         -- delete the folder of the specified name in the parent folder
         del_folder(parent_folder_deleted, To_String(deleted_name));
      else
         -- get deleted folder parent from parameters
         parent_folder_deleted := go_to_folder(current_directory, get_substring_to_string(parameters, 1), True);
         -- get deleted folder name from parameters 
         deleted_name := get_name_from_path(get_substring(parameters, 1));
         -- if no file is found in the parent folder for the specified name, raise an exception
         if find_file(parent_folder_deleted, To_String(deleted_name)) = null then
            raise Invalid_File_Error;
         end if;
         -- delete the file of the specified name in the parent folder
         del_file(parent_folder_deleted, To_String(deleted_name));
      end if;
   end rm_command;
   
   procedure tar_command(current_directory : in out T_Folder; options : in T_Substrings; parameters : in T_Substrings) is
      folder_to_tar : T_Folder; -- folder that we want to tar
      tar_file : T_File; -- the tar file, result of the archiving
      tar_file_name : Unbounded_String; -- the name of the tar file
   begin
      -- R0 : Cree une archive du dossier passe en parametre, ou du dossier courant, ayant la taille de tous les sous-dossiers et de tous les fichiers, sommees, dans le dossier a archiver.
      -- R1 : Si les options transmises ne sont pas gerees, leve une exception(R1.1)
      --      Si le nombre de parametres est incoherent, leve une exception(R1.2)
      --      Si il y a un chemin en parametre, se deplace jusqu'au dossier demande, sinon prend le dossier courant(R1.3)
      --      Cree le nom du fichier a partir du nom du dossier courant(R1.4)
      --      Cree le fichier arechive(R1.5)
      --      Defini la taille du fichier d'archive(R1.6)
      --      Ajoute le fichier au dossier courant(R1.7)
      
      -- R2.1 : Comment R1.1
      --      Si l'option n'est pas prise en compte(R2.1.1)
      --          Leve une exception(R2.1.2)
      --      Fin pour
      -- R2.2 : Comment R1.2
      --      Si il y a un nombre de parametre inatendu(R2.2.1)
      --          Leve une exception(R2.2.2)
      --      Fin pour
      -- R2.3 : Comment R1.3
      --      Si longueur(argument(parameters, 1)) > 0 Alors
      --          Suit le chemin passe en parametre(R2.3.1)
      --      Sinon
      --          dossier_a_archiver <- current_directory
      --      Fin si
      -- R2.4 : Comment R1.4
      --      nom_dossier_a_archiver := (Si est_root(dossier_a_archiver) Alors "root.tar" Sinon nom(dossier_a_archiver) + ".tar")
      -- R2.3 : Comment R1.5
      --      fichier_archive <- creer_fichier(nom_dossier_a_archiver)
      -- R2.4 : Comment R1.6
      --      definir_taille(fichier_archive, calculer_taille_recursivement)
      -- R2.5 : Comment R1.7
      --      ajouter_fichier(current_directory, fichier_archive)
      
      -- R3.1 : Comment R2.1.1
      --      Si contient_seulement_option_supportees(options, "-r") Alors
      -- R3.2 : Comment R2.1.2
      --       Erreur Option_Non_Supportee_Erreur
      -- R3.3 : Comment R2.2.1
      --      Si longueur(parametre) /= 1 Alors
      -- R3.4 : Comment R2.2.2
      --      Erreur Mauvais_Nombre_Arguments_Erreur
      -- R3.5 : Comment R2.3.1
      --      dossier_a_archiver <- aller_au_dossier(current_directory, argument(parameters, 1))
      
      
      
      if get_nb_substrings(options) > 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a source directory is provided, go to this directory
      if get_substring_to_string(parameters, 1)'Length > 0 then
         folder_to_tar := go_to_folder(current_directory, get_substring_to_string(parameters, 1));
      -- else, go to the current directory
      else
         folder_to_tar := current_directory;
      end if;
      -- get name of the tar file
      tar_file_name := (if is_root(folder_to_tar) then To_Unbounded_String("root") else To_Unbounded_String(get_name(folder_to_tar)));
      tar_file_name := tar_file_name & ".tar";
      -- create the tar file
      tar_file := create(To_String(tar_file_name));
      -- set the size of the tar file, corresponding to the size of the folder to tar
      set_size(tar_file, calculate_size(folder_to_tar));
      -- add the tar file to the current directory
      add_file(current_directory, tar_file);
   end tar_command;
   
   procedure clear_command is
   begin
      put(ASCII.ESC & "[2J");
   end clear_command;
   
   procedure help_command (options : in T_Substrings; parameters : in T_Substrings) is
   begin
      if get_nb_substrings(options) /= 0 then
         raise Not_Handled_Option_Error;
      end if;
      if get_nb_substrings(parameters) > 1 then
         raise Wrong_Parameters_Number_Error;
      end if;
      -- if a command is given, print the specific help of this command
      if get_nb_substrings(parameters) = 1 then
         print_specific_help(get_substring_to_string(parameters, 1));
      -- else, print the global help
      else
         print_global_help;
      end if;
   end help_command;

end P_Commands;
