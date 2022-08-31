with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.Strings; use ada.Strings;
with ada.Text_IO; use ada.Text_IO;
with ada.Unchecked_Conversion;
with ada.Numerics.Elementary_Functions; use ada.Numerics.Elementary_Functions;
procedure lab322 is
   package LIIO is new ada.Text_IO.Integer_IO(Long_Integer);
   use LIIO;
   package FIO is new ada.Text_IO.Float_IO(Float);
   use FIO;
   
   
   procedure MyPut(X:Float)is
   begin FIO.Put(X,0,0,0); end;
   
   function IntChar is new ada.Unchecked_Conversion(Character,Long_Integer);
   
   pragma Suppress(overflow_check);
   
   input : File_Type;
   Count : Integer;
   Add,I,OriginalAddress,R,Num : Long_Integer;
   A,Total,MaxF, MinF, AvgF,MaxL,MinL,AvgL : Float;
   type Hash;
   Type HashPointer is access Hash;
   type Hash is record
      Key : String (1..16);
      Address : Long_Integer;
      Probe : Float := 0.0;
   end record;
   H: HashPointer := new Hash;
   type HashTable is array (Long_Integer range <>) of HashPointer;
   HashTableA : HashTable(1 .. 128);
   HashTableB : HashTable(1 .. 128);
   HashTableARand : HashTable(1 .. 128);
   HashTableBRand : HashTable(1 .. 128);
begin
   --building and creating hash table A--
   --------------------------------------------------------------------------
   -----------------------------START 45% LINEAR HASH------------------------
   --------------------------------------------------------------------------
   H := new Hash;
   Open(File => input,
        Mode => In_File,
        Name => "C:\Users\Hadyn\Desktop\\Words200D16.txt");
   Count := 1;
   MaxF := -1.0;
   MaxL := -1.0;
   MinF := 100.0;
   MinL :=100.0;
   AvgF := 0.0;
   AvgL := 0.0;
   Add := 0;
   I := 1;
   while (Count < 59) loop  
      H := new Hash;
      get(input, H.Key);
      for X in 1..16 loop
         Add := Add + IntChar(H.Key(X))*I;
         I := I + 1;
      end loop;
      Add := abs(Add)Mod(128)+1;
      H.Address := Add;
      loop
         if(HashTableA(H.Address) = null)then --if adress we requested is empty then add
            H.Probe := H.Probe + 1.0;
            HashTableA(H.Address) := H;
   
            if(Count < 31)then
               if (HashTableA(H.Address).Probe < MinF) then
                  MinF := HashTableA(H.Address).Probe;
               elsif(HashTableA(H.Address).Probe > MaxF) then
                  MaxF := HashTableA(H.Address).Probe;
               end if;
               AvgF := AvgF + HashTableA(H.Address).Probe;
            else
               if (HashTableA(H.Address).Probe < MinL) then
                  MinL := HashTableA(H.Address).Probe;
               elsif(HashTableA(H.Address).Probe > MaxL) then
                  MaxL := HashTableA(H.Address).Probe;
               end if;
               AvgL := AvgL + HashTableA(H.Address).Probe;
            end if;
   
   
            exit;
         else  --if not move probe until free space/ if probe hits end of mem go back to 1
            if(H.Address < 128) then
               H.Address := H.Address + 1;
               H.Probe :=H.Probe + 1.0;
            else
               H.Address := 1;
               H.Probe := H.Probe + 1.0;
            end if;
         end if;
      end loop;
      Count := Count + 1;
   end loop;
    --probes for first 30
   Put("RESULTS FOR 45% FULL TABLE");
   New_Line;
   Put("Minimum Number of Probes for First 30 Keys: ");
   MyPut(MinF);
   New_Line;
   
   Put("Maximum Number of Probes for First 30 Keys: ");
   MyPut(MaxF);
   New_Line;
   
   AvgF := AvgF/30.0;
   Put("Average Number of Probes for First 30 Keys: ");
   MyPut(AvgF);
   New_Line;
   --probes for last 30
   Put("Minimum Number of Probes for Last 30 Keys: ");
   MyPut(MinL);
   New_Line;
   
   Put("Maximum Number of Probes for Last 30 Keys: ");
   MyPut(MaxL);
   New_Line;
   
   AvgL := AvgL/28.0;
   Put("Average Number of Probes for Last 30 Keys: ");
   MyPut(AvgL);
   New_Line;
   
   I:=1;
   while(I < 129) Loop
      if(HashTableA(I) = null)then
         Put(I);
         Put(" is empty.");
         New_Line;
      else
         Put(I);
         Put(":");  
         Put(HashTableA(I).Key);
         Put(" Probes: ");
         MyPut(HashTableA(I).Probe);
         New_Line;
      end if;
      I := I + 1;
    end loop;
   -- theoretical goes here
   Total := 0.0;
   New_Line;
   Put("Theoretical Expected Number of Linear Probes: ");
   A := 58.0/128.0;
   Total := (1.0-A/2.0)/(1.0-A);
   MyPut(Total);
   New_Line;
   Close(File => input);
   -- Done with Hashtable A
   put("DONE WITH 45% TABLE");
   New_Line;
   New_Line;
   --------------------------------------------------------------------------
   -----------------------------END 45% LINEAR HASH------------------------
   --------------------------------------------------------------------------
   
   
   --------------------------------------------------------------------------
   -----------------------------START 85% LINEAR HASH------------------------
   --------------------------------------------------------------------------
   -- Start with Hashtable B
   H := new Hash;
   Open(File => input,
        Mode => In_File,
        Name => "C:\Users\Hadyn\Desktop\\Words200D16.txt");
   Count := 1;
   MaxF := -1.0;
   MaxL := -1.0;
   MinF := 100.0;
   MinL :=100.0;
   AvgF := 0.0;
   AvgL := 0.0;
   I := 1;
   while (Count < 110) loop  
      H := new Hash;
      get(input, H.Key);
      for X in 1..16 loop
         Add := Add + IntChar(H.Key(X))*I;
         I := I+1;
      end loop;
      Add := abs(Add)Mod(128)+1;
      H.Address := Add;
      loop
         if(HashTableB(H.Address) = null)then --if adress we requested is empty then add
            H.Probe := H.Probe + 1.0;
            HashTableB(H.Address) := H;
   
            if(Count < 31)then
               if (HashTableB(H.Address).Probe < MinF) then
                  MinF := HashTableB(H.Address).Probe;
               elsif(HashTableB(H.Address).Probe > MaxF) then
                  MaxF := HashTableB(H.Address).Probe;
               end if;
               AvgF := AvgF + HashTableB(H.Address).Probe;
            elsif(Count > 78)then
               if (HashTableB(H.Address).Probe < MinL) then
                  MinL := HashTableB(H.Address).Probe;
               elsif(HashTableB(H.Address).Probe > MaxL) then
                  MaxL := HashTableB(H.Address).Probe;
               end if;
               AvgL := AvgL + HashTableB(H.Address).Probe;
            end if;
   
   
            exit;
         else  --if not move probe until free space/ if probe hits end of mem go back to 1
            if(H.Address < 128) then
               H.Address := H.Address + 1;
               H.Probe :=H.Probe + 1.0;
            else
               H.Address := 1;
               H.Probe := H.Probe + 1.0;
            end if;
         end if;
      end loop;
      Count := Count + 1;
   end loop;
   --probes for first 30
   put("RESULTS FOR 85% FULL TABLE");
   New_Line;
   Put("Minimum Number of Probes for First 30 Keys: ");
   MyPut(MinF);
   New_Line;
   
   Put("Maximum Number of Probes for First 30 Keys: ");
   MyPut(MaxF);
   New_Line;
   
   AvgF := AvgF/30.0;
   Put("Average Number of Probes for First 30 Keys: ");
   MyPut(AvgF);
   New_Line;
   --probes for last 30
   Put("Minimum Number of Probes for Last 30 Keys: ");
   MyPut(MinL);
   New_Line;
   
   Put("Maximum Number of Probes for Last 30 Keys: ");
   MyPut(MaxL);
   New_Line;
   
   AvgL := AvgL/30.0;
   Put("Average Number of Probes for Last 30 Keys: ");
   MyPut(AvgL);
   New_Line;
   
   I:=1;
   while(I < 129) Loop
      if(HashTableB(I) = null)then
         Put(I);
         Put(" is empty.");
         New_Line;
      else
         Put(I);
         Put(":");  
         Put(HashTableB(I).Key);
         Put(" Probes: ");
         MyPut(HashTableB(I).Probe);
         New_Line;
      end if;
      I := I + 1;
   end loop;
   
   -- calc probes
    Total := 0.0;
   New_Line;
   Put("Theoretical Expected Number of Linear Probes: ");
   A := 109.0/128.0;
   Total := (1.0-A/2.0)/(1.0-A);
   MyPut(Total);
   New_Line;
   
   put("DONE WITH 85% TABLE");
   New_Line;
   New_Line;
   
   Close(File => input);
   --Done with HashTableB
   
   --------------------------------------------------------------------------
   -----------------------------END 85% LINEAR HASH------------------------
   --------------------------------------------------------------------------
   
   --BEGINNING RANDOM HASH
   
   
   --------------------------------------------------------------------------
   -----------------------------START 45% RANDOM HASH------------------------
   --------------------------------------------------------------------------
   -- start 45% random Hash
   H := new Hash;
   Open(File => input,
        Mode => In_File,
        Name => "C:\Users\Hadyn\Desktop\\Words200D16.txt");
   Count := 1;
   MaxF := -1.0;
   MaxL := -1.0;
   MinF := 100.0;
   MinL :=100.0;
   AvgF := 0.0;
   AvgL := 0.0;
   R := 1;
   I := 1;
   while (Count < 59) loop  
      H := new Hash;
      get(input, H.Key);
      for X in 1..16 loop
         Add := Add + IntChar(H.Key(X))*I;
         I:=I+1;
      end loop;
      Add := abs(Add)Mod(128)+1;
      H.Address := Add;
      OriginalAddress := H.Address;
      R := 1;
      loop
         if(HashTableARand(H.Address) = null)then --if adress we requested is empty then add
            H.Probe := H.Probe + 1.0;
            HashTableARand(H.Address) := H;
            
            if(Count < 31)then   --counting first 30 probes
               if (HashTableARand(H.Address).Probe < MinF) then
                  MinF := HashTableARand(H.Address).Probe;
               elsif(HashTableARand(H.Address).Probe > MaxF) then
                  MaxF := HashTableARand(H.Address).Probe;
               end if;
               AvgF := AvgF + HashTableARand(H.Address).Probe;
            else
               if (HashTableARand(H.Address).Probe < MinL) then
                  MinL := HashTableARand(H.Address).Probe;
               elsif(HashTableARand(H.Address).Probe > MaxL) then
                  MaxL := HashTableARand(H.Address).Probe;
               end if;
               AvgL := AvgL + HashTableARand(H.Address).Probe;
            end if;   
               
                     
            exit;
         else  --if not move probe with random num gen 
            H.Probe := H.Probe +1.0;
            R := (5 * R)Mod(2**(9));
            Num:= R/4;
            exit when (R = 1);
            H.Address := OriginalAddress;
            H.Address := H.Address+Num;
            if(H.Address > 128)then
               H.Address := H.Address-128;
            end if;
         end if;
      end loop;   
      Count := Count + 1;
   end loop;
    --probes for first 30
   Put("RESULTS FOR 45% FULL RANDOM TABLE");
   New_Line;
   Put("Minimum Number of Probes for First 30 Keys: ");
   MyPut(MinF);
   New_Line;
   
   Put("Maximum Number of Probes for First 30 Keys: ");
   MyPut(MaxF);
   New_Line;
   
   AvgF := AvgF/30.0;
   Put("Average Number of Probes for First 30 Keys: ");
   MyPut(AvgF);
   New_Line;
   --probes for last 30
   Put("Minimum Number of Probes for Last 30 Keys: ");
   MyPut(MinL);
   New_Line;
   
   Put("Maximum Number of Probes for Last 30 Keys: ");
   MyPut(MaxL);
   New_Line;
   
   AvgL := AvgL/28.0;
   Put("Average Number of Probes for Last 30 Keys: ");
   MyPut(AvgL);
   New_Line;
   
   I:=1;
   while(I < 129) Loop
      if(HashTableARand(I) = null)then
         Put(I);
         Put(" is empty.");
         New_Line;
      else
         Put(I);
         Put(":");  
         Put(HashTableARand(I).Key);
         Put(" Probes: ");
         MyPut(HashTableARand(I).Probe);
         New_Line;
      end if;
      I := I + 1;
    end loop;
   
   Total := 0.0;
   New_Line;
   Put("Theoretical Expected Number of Random Probes: ");
   A := 58.0/128.0;
   Total := -(1.0/A)*Log(X => 1.0-A);
   MyPut(Total);
   New_Line;
   -- theoretical goes here
   
   Close(File => input);
   -- Done with Hashtable A Random
   put("DONE WITH 45% RANDOM TABLE ");
   New_Line;
   New_Line;
   
   --------------------------------------------------------------------------
   -----------------------------END 45% RANDOM HASH------------------------
   --------------------------------------------------------------------------
   
   
   
   --------------------------------------------------------------------------
   -----------------------------START 85% RANDOM HASH------------------------
   --------------------------------------------------------------------------
   
   
   
   H := new Hash;
   Open(File => input,
        Mode => In_File,
        Name => "C:\Users\Hadyn\Desktop\\Words200D16.txt");
   Count := 1;
   MaxF := -1.0;
   MaxL := -1.0;
   MinF := 100.0;
   MinL :=100.0;
   AvgF := 0.0;
   AvgL := 0.0;
   R := 1;
   I := 1;
   while (Count < 110) loop  
      H := new Hash;
      get(input, H.Key);
      for X in 1..16 loop
         Add := Add + IntChar(H.Key(X))*I;
         I := I+1;
      end loop;
      Add := abs(Add)Mod(128)+1;
      H.Address := Add;
      OriginalAddress := H.Address;
      R := 1;
      loop
         if(HashTableBRand(H.Address) = null)then --if adress we requested is empty then add
            H.Probe := H.Probe + 1.0;
            HashTableBRand(H.Address) := H;
   
            if(Count < 31)then
               if (HashTableBRand(H.Address).Probe < MinF) then
                  MinF := HashTableBRand(H.Address).Probe;
               elsif(HashTableBRand(H.Address).Probe > MaxF) then
                  MaxF := HashTableBRand(H.Address).Probe;
               end if;
               AvgF := AvgF + HashTableBRand(H.Address).Probe;
            elsif(Count > 78)then
               if (HashTableBRand(H.Address).Probe < MinL) then
                  MinL := HashTableBRand(H.Address).Probe;
               elsif(HashTableBRand(H.Address).Probe > MaxL) then
                  MaxL := HashTableBRand(H.Address).Probe;
               end if;
               AvgL := AvgL + HashTableBRand(H.Address).Probe;
            end if;
               
   
   
            exit;
         else  --if not move probe until free space/ if probe hits end of mem go back to 1
            H.Probe := H.Probe + 1.0;
            R := (5 * R)Mod(2**(9));
            Num:= R/4;
            exit when (R = 1);
            H.Address := OriginalAddress;
            H.Address := H.Address+Num;
            if(H.Address > 128)then
               H.Address := H.Address-128;
            end if;
         end if;
      end loop;
      Count := Count + 1;
   end loop;
   --probes for first 30
   put("RESULTS FOR 85% FULL RANDOM TABLE");
   New_Line;
   Put("Minimum Number of Probes for First 30 Keys: ");
   MyPut(MinF);
   New_Line;
   
   Put("Maximum Number of Probes for First 30 Keys: ");
   MyPut(MaxF);
   New_Line;
   
   AvgF := AvgF/30.0;
   Put("Average Number of Probes for First 30 Keys: ");
   MyPut(AvgF);
   New_Line;
   --probes for last 30
   Put("Minimum Number of Probes for Last 30 Keys: ");
   MyPut(MinL);
   New_Line;
   
   Put("Maximum Number of Probes for Last 30 Keys: ");
   MyPut(MaxL);
   New_Line;
   
   AvgL := AvgL/30.0;
   Put("Average Number of Probes for Last 30 Keys: ");
   MyPut(AvgL);
   New_Line;
   
   I:=1;
   while(I < 129) Loop
      if(HashTableBRand(I) = null)then
         Put(I);
         Put(" is empty.");
         New_Line;
      else
         Put(I);
         Put(":");  
         Put(HashTableBRand(I).Key);
         Put(" Probes: ");
         MyPut(HashTableBRand(I).Probe);
         New_Line;
      end if;
      I := I + 1;
   end loop;
   
   Total := 0.0;
   New_Line;
   Put("Theoretical Expected Number of Random Probes: ");
   A := 109.0/128.0;
   Total := -(1.0/A)*Log(X => 1.0-A);
   MyPut(Total);
   New_Line;
   -- calc probes
   put("DONE WITH 85% RANDOM TABLE");
   New_Line;
   New_Line;
   
 
   
   Close(File => input);
   
            
   --------------------------------------------------------------------------
   -----------------------------END 85% RANDOM HASH------------------------
   --------------------------------------------------------------------------
         
      
        
   
   
end lab322;
