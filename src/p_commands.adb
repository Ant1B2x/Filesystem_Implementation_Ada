package body P_Commands is

   procedure pwdCommand(firstParameter: String; currentDirectory: T_Folder) is
      absolutePath: Unbounded_String;
      current: T_Folder;
   begin
      return  get_path(currentDirectory) & "/" & get_name(currentDirectory);
   end pwdCommand;
   
   procedure lsCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder)is
   begin
      if(firstParameter = "-r")then
         for i in 1.. get_nb_folders(currentDirectory) loop
            lsRCommand("/", get_folder(currentDirectory,i));
         end loop;
      else
         Put_Line("Actuel :" & currentDirectory.donnee.metadata.Nom);
         for i in 1.. currentDirectory.all.nbFils loop
            Put_Line("    Dossier => " & currentDirectory.all.fils(i).all.donnee.metadata.Nom);
         end loop;
         
         for i in 1.. currentDirectory.all.donnee.nb_fichiers loop
            Put_Line("    Fichier => " & currentDirectory.all.donnee.fichiers(i).metaData.Nom);
         end loop;
      end if;
   end lsCommand;
   
   procedure lsRCommand(precedingPath: String; currentDirectory: T_Folder)is
      -- Pour colorier : https://docs.adacore.com/gnatcoll-docs/terminals.html
      -- with GNATCOLL.Terminal;  use GNATCOLL.Terminal;
      -- Info.Set_Color (Standard_Output, Blue);
      -- Put_Line ("A blue on yellow line");
      -- Info.Set_Color (Standard_Output, Style => Reset_All);
      currentPath: String;
   begin
      currentPath := precedingPath & "/" & currentDirectory.all.donnee.metadata.Nom;
      Put_Line(currentPath);
      for i in 1.. get_nb_folders(currentDirectory) loop
         Put_Line(get_name(get_folder(currentDirectory,i)));
      end loop;
      for i in 1.. get_nb_files(currentDirectory) loop
         Put_Line(get_name(get_name(currentDirectory,i)));
      end loop;
      for i in 1.. get_nb_folders(currentDirectory) loop
         lsRCommand(currentPath);
      end loop;
   end lsRCommand;
   
   procedure rmCommand(OptionTrue : Boolean;firstParameter: String; currentDirectory: in out T_Folder)is
      trouve: Boolean := False;
   begin
      if(OptionTrue)then
         delFilsDossier(currentDirectory,findFilsDossier(currentDirectory,firstParameter,trouve));
      else
         delFichier(currentDirectory,findFichierDossier(currentDirectory,firstParameter,trouve));
      end if;
   end rmCommand;
   procedure pwdCommand(currentDirectory: T_Folder)is
   begin
      Put_Line(currentDirectory.all.donnee.metadata.Nom);
   end pwdCommand;
   procedure cdCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure mkdirCommand(firstParameter: String; currentDirectory: in out T_Folder)is
      fils : T_Folder;
   begin
      fils := creerDossier(firstParameter);
      addFilsDossier(currentDirectory,fils);
   end mkdirCommand;
   procedure cpCommand(OptionTrue : Boolean; firstParameter: String; currentDirectory: T_Folder);
   procedure mvCommand(firstParameter: String; secondParameter: String; currentDirectory: T_Folder);
   procedure tarCommand(firstParameter: String; currentDirectory: T_Folder);
   procedure touchCommand(firstParameter: String; currentDirectory: T_Folder)is
      fichier : T_Fichier;
   begin
      fichier := creerFichier(firstParameter);
      addFichier(dossier => currentDirectory,file => fichier);
   end touchCommand;
   
   function "<" (L, R : sonRecord) return Boolean is
   begin
      if L.Name < R.Name then return True;
      elsif L.Name = R.Name then return L.Value < R.Value;
      else return False;
      end if;
   end "<";

end P_Commands;
