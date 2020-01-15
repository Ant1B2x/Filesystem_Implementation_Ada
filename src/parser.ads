package parser is

   procedure run_command(current_dir: T_Folder; command: String);
   
private
   type encoded_commands is (ls, rm, pwd, cd, mkdir, cp, mv, tar, touch);

end parser;
