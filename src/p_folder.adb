package body p_folder is

   function is_empty (folder : in T_Folder) return Boolean is
   begin
      return P_Folder_Tree.is_empty(folder);
   end is_empty;

   
   
   
   
   
   
   
   
   
end p_folder;
