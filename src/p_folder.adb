package body P_Folder is
  
   function create_root return T_Folder is
      folder : T_Folder; -- the returned root folder
      data : T_Folder_Data; -- the data of the root folder
   begin
      -- initialize the folder tree
      folder := P_Folder_Tree.create;
      -- initialize the file array
      data.files := P_Files.create;
      -- initialize the folder metadata
      data.metadata := P_Metadata.create_root;
      -- set the folder data on the folder
      P_Folder_Tree.set_data(folder, data);
      -- return the root folder
      return folder;
   end create_root;
   
   function create (name : in String; parent : in out T_Folder; rights : in T_Rights := (RWX, RX, RX)) return T_Folder is
      folder : T_Folder; -- the returned new folder
      data : T_Folder_Data; -- the data of the returned new folder
   begin
      -- if the parent contains a folder or a file with the same name than the new folder, raise an Exception
      if has_son_with_this_name(parent, name) then
         raise Same_Name_Error;
      end if;
      -- if the parent directory is already full of folders, raise an Exception
      if get_nb_folders(parent) = NMAX_VALUES then
         raise Full_Folder_Error;
      end if;
      -- initialize the folder tree
      folder := P_Folder_Tree.create(parent);
      -- initialize the file array
      data.files := P_Files.create;
      -- initialize the folder metadata, FOLDER_SIZE is set automatically and is equal to 10Ko, the absolute path is calculated
      data.metadata := P_Metadata.create(name, rights, FOLDER_SIZE, calculate_path(folder));
      -- set the folder data on the folder
      P_Folder_Tree.set_data(folder, data);
      -- return the new folder
      return folder;
   end create;
 
   function get_name (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_name(P_Folder_Tree.get_data(folder).metadata);
   end get_name;
   
   procedure set_name (folder : in out T_Folder; name : in String) is
      data : T_Folder_Data; -- data of the folder
   begin
      -- we should do it like this because T_Folder_Data is private
      -- get the data of the folder
      data := P_Folder_Tree.get_data(folder);
      -- modify the name in the folder data
      P_Metadata.set_name(data.metadata, name);
      -- set the new data of the folder
      P_Folder_Tree.set_data(folder, data);
   end set_name;
   
   function get_rights (folder : in T_Folder) return T_Rights is
   begin
      return P_Metadata.get_rights(P_Folder_Tree.get_data(folder).metadata);
   end get_rights;
   
   procedure set_rights (folder : in out T_Folder; rights : in T_Rights) is
      data : T_Folder_Data; -- data of the folder
   begin
      -- we should do it like this because T_Folder_Data is private
      -- get the data of the folder
      data := P_Folder_Tree.get_data(folder);
      -- modify the rights in the folder data
      P_Metadata.set_rights(data.metadata, rights);
      -- set the new data of the folder
      P_Folder_Tree.set_data(folder, data);
   end set_rights;
   
   function get_size (folder : in T_Folder) return Integer is
   begin
      return P_Metadata.get_size (P_Folder_Tree.get_data(folder).metadata);
   end get_size;
   
   function get_root return T_Folder is
   begin
      -- singleton pattern
      -- if root does not exist, initialize it
      if ROOT = null then
         ROOT := create_root;
      end if;
      -- return root anyway
      return ROOT;
   end get_root;
   
   function calculate_path (folder : in T_Folder) return String is
      absolute_path : Unbounded_String; -- absolute path from root to folder
      current : T_Folder; -- current folder
   begin
      -- R0 : Définir le chemin menant à un dossier, depuis celui-ci
      -- R1 : Si je suis le dossier originel, je retourne '/'(R1.1)
      --      Récupérer le dossier courrant(R1.2)
      --      Remonter jusqu'au dossier originel en sauvegardant dans le chemin les différents dossier traversés(R1.3)
      
      -- R2.1 : Comment R1.1
      --     Si is_root(folder) Alors
      --         Retour '/'
      --     Fin si
      -- R2.2 : Comment R1.2
      --     current <- folder
      -- R2.2 : Comment R1.3
      --     Tant que je ne suis pas dans le dossier originel Faire(R2.2.1)
      --         Remonter au dossier précédent(R2.2.2)
      --         Sauvegarder le nom du dossier courrant dans le chemin(R2.2.3)
      --     Fin tant que
      
      -- R3.1 : Comment (R2.2.1)
      --     non non_null(parent(current)) et ensuite non est_originel(parent(current)
      -- R3.2 : Comment (R2.2.2)
      --     current <- parent(current)
      -- R3.3 : Comment (R2.2.3)
      --     chemin <- '/' + nom(current) + chemin
      
      
      -- if the parent is root, it's a specific case, return "/"
      if is_root(get_parent(folder)) then
         return ""&FILE_SEPARATOR;
      end if;
      -- initialize absolute path and current folder
      absolute_path := To_Unbounded_String("");
      current := folder;
      -- while the current parent is not null and the current parent is not root
      while not is_null(get_parent(current)) and then not is_root(get_parent(current)) loop
         -- current becomes its parent
         current := get_parent(current);
         -- add "/" + name of current at the beggining of absolute path
         absolute_path := FILE_SEPARATOR & get_name(current) & absolute_path;
      end loop;
      -- return absolute calculated path
      return To_String(absolute_path);
   end calculate_path;
   
   function get_path (folder : in T_Folder) return String is
   begin
      return P_Metadata.get_path (P_Folder_Tree.get_data(folder).metadata);
   end get_path;
   
   function get_pwd (folder : in T_Folder) return String is
   begin
      -- R0 : Obtenir pwd
      -- R1 : Si je suis le dossier originel(R1.1)
      --      Si mon parent est le dossier originel(R1.2)
      --      Sinon(R1.3)
      
      -- R2.1 : Comment R1.1
      --     Si is_root(folder) Alors
      --         Retour '/'
      --     Fin si
      -- R2.2 : Comment R1.2
      --     Si is_root(parent(folder)) Alors
      --         Retour chemin(folder) + nom(folder)
      --     Fin si
      -- R2.2 : Comment R1.3
      --     Tant que je ne suis pas dans le dossier originel Faire(R2.2.1)
      --         Retour chemin(folder) + '/' + nom(folder)
      --     Fin tant que
      
      
      -- if the current directory is root
      if is_root(folder) then 
         -- return "" & "/"
         return get_path(folder) & FILE_SEPARATOR; 
      -- if the current directory is a root subdirectory
      elsif is_root(get_parent(folder)) then 
         -- return "/" & name
         return get_path(folder) & get_name(folder);
      -- in every other directory
      else
         -- return path & "/" & name
         return get_path(folder) & FILE_SEPARATOR & get_name(folder);
      end if;
   end get_pwd;
   
   function get_parent (folder : in T_Folder) return T_Folder is
   begin
      return P_Folder_Tree.get_parent(folder);
   end get_parent;
   
   function is_empty (folder : in T_Folder) return Boolean is
   begin
      -- return True if there is no folder and no file in the given folder
      return get_nb_folders(folder) = 0 and get_nb_files(folder) = 0;
   end is_empty;
   
   function is_null (folder : in T_Folder) return Boolean is
   begin
      return P_Folder_Tree.is_null(folder);
   end is_null;
   
   function is_root (folder : in T_Folder) return Boolean is
   begin
      -- in our implementation, only root has a null parent
      return get_parent(folder) = null;
   end is_root;
   
   function are_the_same(folder1 : T_Folder; folder2 : T_Folder) return Boolean is
   begin
      -- if the pwd of folders is equal, that means they are the same
      return get_pwd(folder1) = get_pwd(folder2);
   end are_the_same;
   
   function get_data (folder : in T_Folder) return T_Folder_Data is
   begin
      return P_Folder_Tree.get_data(folder);
   end get_data;
   
   procedure set_data (folder : in out T_Folder; folder_data : in T_Folder_Data) is
   begin
      P_Folder_Tree.set_data(folder, folder_data);
   end set_data;
   
   function get_folder (folder : in T_Folder; index : in Integer) return T_Folder is
   begin
      return P_Folder_Tree.get_sibling(folder, index);
   end get_folder;
   
   function get_nb_folders (folder : in T_Folder) return Integer is
   begin
      return P_Folder_Tree.get_nb_siblings(folder);
   end get_nb_folders;
   
   function find_folder (current_folder : in T_Folder; folder_name : in String) return T_Folder is
   begin
      -- R0 : Trouver un sous-dossier direct depuis un nom
      -- R1 : Si le nom est '.'(R1.1)
      --      Si le nom est '..'(R1.2)
      --      Sinon(R1.3)
      
      -- R2.1 : Comment R1.1
      --     Si folder_name = '.' Alors
      --         Retour current_folder
      --     Fin si
      -- R2.2 : Comment R1.2
      --     Si folder_name = '..' Alors
      --         Retour parent(current_folder)
      --     Fin si
      -- R2.2 : Comment R1.3
      --     Pour tout les fils du dossier courant Faire(R2.2.1)
      --         Vérifier si c'est le dossier que je recherche(R2.2.2)
      --     Fin tant que
      --     Non trouvé(R2.2.3)
      
      -- R3.1 : Comment (R2.2.1)
      --     index allant de 1 à nombre_fils(current_folder)
      -- R3.2 : Comment (R2.2.2)
      --     Si nom(dossier_fils(current_folder, index)) = folder_name
      --         Retour dossier_fils(current_folder, index)
      --     Fin si
      -- R3.3 : Comment (R2.2.3)
      --     Retour null
      
      -- if the folder name is ".", return the current folder itself
      if folder_name = "." then
         return current_folder;
      end if;
      -- if the folder name is ".."
      if folder_name = ".." then
         -- if the current folder is root, return itself
         if is_root(current_folder) then
            return current_folder;
         -- else, return the folder parent
         else
            return get_parent(current_folder);
         end if;
      end if;
      -- for all sub-folders
      for i in 1..get_nb_folders(current_folder) loop
         -- if the sub-folder name is equal to the given folder_name
         if get_name ( get_folder(current_folder, i) ) = folder_name then
            -- return the sub-folder
            return get_folder(current_folder, i);
         end if;
      end loop;
      -- anyway, return null
      return null;
   end find_folder;
   
   procedure del_folder (folder : in out T_Folder; folder_name : in String) is
      folder_sibling : T_Folder; -- the potentially deleted sub-folder
   begin
      -- R0 : Supprimer un sous-dossier direct depuis un nom
      -- R1 : Pour tout les fils du dossier courant Faire(R1.1)
      --         Vérifier si c'est le dossier que je recherche(R1.2)
      --     Fin tant que
      
      -- R2.1 : Comment (R1.1)
      --     index allant de 1 à nombre_fils(current_folder)
      -- R2.2 : Comment (R1.2)
      --     Si nom(dossier_fils(current_folder, index)) = folder_name
      --         Supprimer fils(R2.2.1)
      --     Fin si
      
      -- R3.1 : Comment R2.2.1
      --    enfant <- dossier_fils(folder, i);
      --    -----> Voir P_Folder_Tree.del_sibling
      
      -- for all sub-folders
      for i in 1..get_nb_folders(folder) loop
         -- if the sub-folder name is equal to the given folder_name
         if get_name(get_folder(folder, i)) = folder_name then
            -- get the sub-folder using get_folder
            folder_sibling := get_folder(folder, i);
            -- deleted the sub-folder in the folder tree
            P_Folder_Tree.del_sibling(folder, folder_sibling);
         end if;
      end loop;
   end del_folder;
   
   function get_file (folder : in T_Folder; index : in Integer) return T_File is
   begin
      return P_Files.get_value( P_Folder_Tree.get_data(folder).files, index);
   end get_file;
   
   function get_nb_files (folder : in T_Folder) return Integer is
   begin
      return P_Files.get_nb_values (P_Folder_Tree.get_data(folder).files);
   end get_nb_files;
   
   function find_file (current_folder : in T_Folder; file_name : in String) return T_File is
   begin
      -- R0 : Trouver un fichier direct depuis un nom
      -- R1 : Pour tout les fichiers du dossier courant Faire(R2.2.1)
      --         Vérifier si c'est le fichiers que je recherche(R2.2.2)
      --     Fin tant que
      --     Non trouvé(R2.2.3)
      
      -- R3.1 : Comment (R2.2.1)
      --     index allant de 1 à nombre_fichiers(current_folder)
      -- R3.2 : Comment (R2.2.2)
      --     Si nom(fichier(current_folder, index)) = file_name
      --         Retour fichier(current_folder, index)
      --     Fin si
      -- R3.3 : Comment (R2.2.3)
      --     Retour null
      
      
      -- for all files
      for i in 1..get_nb_files(current_folder) loop
         -- if the file name is equal to the given file_name
         if P_File.get_name(get_file(current_folder, i)) = file_name then
            -- return the file
            return get_file(current_folder, i);
         end if;
      end loop;
      -- anyway, return null
      return null;
   end find_file;
   
   procedure add_file (folder : in out T_Folder; file : in T_File) is
      folder_data : T_Folder_Data; -- the data of the folder
      new_file : T_File; -- new file to add in the folder, clone of file parameter
   begin
      -- R0 : Ajouter un fichier au dossier
      -- R1 : Si j'ai un fils avec le même nom(R1.1)
      --      Si j'ai déjà atteint ma limite de fils(R1.2)
      --      Sinon(R1.3)
      
      -- R2.1 : Comment (R1.1)
      --     Si fils_avec_meme_nom(folder, nom(file)) Alors
      --         Erreur Fils_Avec_Meme_Nom_Erreur
      --     Fin si
      -- R2.2 : Comment (R1.2)
      --     Si nombre_fils(folder) = NOMBRE_MAX_FILS Alors
      --         Erreur Dossier_Plein_Erreur
      --     Fin si
      -- R2.3 : Comment (R1.3)
      --     Recuperer les donnees du fichier(R2.3.1)
      --     Cloner le fichier(R2.3.2)
      --     Ajouter le fichier clone au dossier(R2.3.3)
      --     Ajouter les donnees au fichier clone(R2.3.4)
      
      -- R3.1 : Comment R2.3.1
      --     donnes_fichier <- donnees(fichier)
      -- R3.1 : Comment R2.3.2
      --     clone_fichier <- clone(fichier)
      -- R3.1 : Comment R2.3.3
      --     -----> Voir P_Files.add_value
      -- R3.1 : Comment R2.3.4
      --     definir_donnees(fichier, donnees_fichier)
      
      -- if the parent contains a folder or a file with the same name than the new file, raise an Exception
      if has_son_with_this_name(folder, P_File.get_name(file)) then
         raise Same_Name_Error;
      end if;
      -- if the parent directory is already full of files, raise an Exception
      if get_nb_files(folder) = NMAX_VALUES then
         raise Full_Folder_Error;
      end if;
      -- get the data of the folder
      folder_data := get_data(folder);
      -- clone the file parameter, change its path to the pwd of the folder
      new_file := clone(file, get_pwd(folder));
      -- add the new file to the files array of the folder
      P_Files.add_value(folder_data.files, new_file);
      -- set the modified data of the folder
      set_data(folder, folder_data);
   end add_file;
   
   procedure del_file (folder : in out T_Folder; file_name : in String) is
      folder_data : T_Folder_Data; -- the data of the folder
   begin
      -- R0 : Supprimer un fichier direct depuis un nom
      -- R1 : On recupere les donnees qui contiennent les fichiers(R1.1)
      --      On supprime le fichier des donnees(R1.2)
      --      On redonne les donnees au fichier(R1.3)
      
      -- R2.1 : Comment R1.1
      --      donnees_dossier <- donnees(folder)
      -- R2.2 : Comment R1.2
      --      Pour tout les fichiers de donnees_dossier Faire(R1.1)
      --         Vérifier si c'est le fichier que je recherche(R1.2)
      --     Fin tant que
      -- R2.3 : Comment R1.3
      --     definir_donnees(folder, donnees_dossier);
      
      -- R3.1 : Comment (R1.1)
      --     index allant de 1 à nombre_fichiers(donnees_dossier.fichiers)
      -- R3.2 : Comment (R1.2)
      --     Si nom(fichier(donnees_dossier.fichiers, index)) = file_name
      --         Supprimer fils(R2.2.1)
      --     Fin si
      
      -- R3.1 : Comment R2.2.1
      --    -----> Voir P_Files.del_value
      
      
      -- get the data of the folder
      folder_data := get_data(folder);
      -- for all files
      for i in 1..P_Files.get_nb_values(folder_data.files) loop
         -- if the file name is equal to the given file name
         if P_File.get_name( P_Files.get_value(folder_data.files, i) ) = file_name then
            -- delete the file
            P_Files.del_value(folder_data.files, P_Files.get_value(folder_data.files, i));
         end if;
      end loop;
      -- set the modified data of the folder
      set_data(folder, folder_data);
   end del_file;
   
   function has_son_with_this_name(folder: T_Folder; name: String) return Boolean is
   begin
      -- R0 : Trouve si le dossier a deja un fils du meme nom
      -- R1 : Retourne vrai Si il a un dossier du meme nom(R1.1)
      --      Retourne vrai si il a un fichier du meme nom(R1.2)
      --      Retourne faux sinon(R1.2)
      -- R2.1 : Comment R1.1
      --      Si dossier(folder, name) Alors
      --          Retourne vrai
      --      Fin si
      -- R2.2 : Comment R1.2
      --      Si fichier(folder, name) Alors
      --          Retourne vrai
      --      Fin si
      -- R2.2 : Comment R1.2
      --      Retourne vrai
      
      -- return true if a folder or a file is found for the given name
      return find_folder(folder, name) /= null or find_file(folder, name) /= null;
   end has_son_with_this_name;
   
   function has_parent(folder : in T_Folder; supposed_parent : in T_Folder) return Boolean is
      current : T_Folder; -- current folder
   begin
      -- R0 : Trouve si le dossier ce parent comme ancetre
      -- R1 : Prend folder comme dossier courant(R1.1)
      --      Parcours tous les parents tant que mon parent n'est pas le dossier originel et que le parent recherche n'est pas rencontre(R1.2)
      --      Retourne si le courant est le dossier recheche comme parent(R1.3)
      -- R2.1 : Comment R1.1
      --      courant <- folder
      -- R2.2 : Comment R1.2
      --      Tant que mon parent n'est pas le dossier originel et que courant n'est pas le parent recherche Faire(R2.2.1)
      --          Je passe au parent suivant(R2.2.2)
      --      Fin tant que
      -- R2.3 : Comment R1.3
      --      Retour (supposed_parent, courant)
      
      -- R3.1 : Comment R2.2.1
      --      non est_dossier_originel(courant) et non egaux(supposed_parent, courant)
      -- R3.2 : Comment R2.2.2
      --      courant <- parent(courant)
      
      -- set the current to folder
      current := folder;
      -- while current is not root and current is different to supposed parent
      while not is_root(current) and not are_the_same(supposed_parent, current) loop
         -- current becomes its parent
         current := get_parent(current);
      end loop;
      -- finally, return True if supposed parent is equal to current, False otherwise
      return are_the_same(supposed_parent, current);
   end has_parent;
   
end P_Folder;
