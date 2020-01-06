package P_Metadata is
   
   SMAX_STRING : constant Integer := 100;
   
   type T_Rights is private;
   type t_metadata is record
      name : String(1..SMAX_STRING);
      rights : T_Rights;
      size : Long_Integer;
   end record;
   
   -- get droits, set droits, getnom, setnom, getsize, setsize
   
private
   
   type T_Rights is (R,W,X,RW,RX,WX,RWX);       
      
end P_Metadata;
