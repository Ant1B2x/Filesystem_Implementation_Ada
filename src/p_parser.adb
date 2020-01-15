package body p_parser is

   procedure runCommand(current_dir: T_Folder; command: String) is 
 
      Subs : T_SubStrings;
      Seps : Character := ' ';  
      OptionTrue : Boolean := False;
      firstParameter : Unbounded_String;
      secondParameter : Unbounded_String;
      numberOfElement : Integer;
      
 
   begin
      Subs := splitString(StringToBeSplit => command,Separator => Seps);

      numberOfElement := Subs.nbSubStrings;
      if(numberOfElement > 1)then
         if(getSubStringToString(Subs, 2) = "-r")then
            OptionTrue := True;
            if(numberOfElement > 2)then
               firstParameter := Subs.subStrings(3);
               if(numberOfElement = 4)then
                  secondParameter := Subs.subStrings(4);
               end if;
            end if;
         else
            firstParameter := Subs.subStrings(2);
            if(numberOfElement = 3)then
               secondParameter := Subs.subStrings(3);
            end if;
         end if;
      else
         firstParameter := To_Unbounded_String("");
         secondParameter := To_Unbounded_String("");
      end if;
      case encodedCommands'Value(getSubStringToString(Subs, 1)) is
         when ls => lsCommand(OptionTrue, To_String(firstParameter), current_dir);
         when rm => rmCommand(OptionTrue,To_String(firstParameter), current_dir);
         when pwd => pwdCommand(current_dir);
         when  cd => cdCommand(To_String(firstParameter), current_dir);
         when mkdir => mkdirCommand(To_String(firstParameter), current_dir);
         when  cp => cpCommand(OptionTrue, To_String(firstParameter), current_dir);
         when  mv => mvCommand(To_String(firstParameter), To_String(secondParameter), current_dir);
         when  tar => tarCommand(To_String(firstParameter), current_dir);
         when  touch => touchCommand(To_String(firstParameter), current_dir);
         when others => Put_Line("> Commande non reconnue.");
      end case;
   end runCommand;

end p_parser;
