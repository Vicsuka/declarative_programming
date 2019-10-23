-module(khf1).
-author('vicsuka@gmail.com').
-vsn('2019-10-14').
-export([feldarabolasa/2]).
-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1]).


-type matrix() :: [row()].
-type row() :: [any()].
-type parameter() :: {subRows(), subCols()}.
-type subRows() :: integer().
-type subCols() :: integer().
-spec khf1:feldarabolasa(Mss :: matrix(), P :: parameter()) -> Lss :: [[any()]].

feldarabolasa(Matrixom, Prm)-> feldarabolasi(Matrixom,Prm).
   
feldarabolasi(Mx,Prm) -> 
   MaxCol = length(hd(Mx)),
   MaxRow = length(Mx),
   {RowNumber,_} = Prm,
   {_,ColNumber} = Prm,

   % if (MaxRow =< RowNumber) -> 

   Result = createSubLists(Mx,RowNumber,ColNumber,MaxCol,MaxRow,0,0,[],[],0,0),
   IsResultList = is_list(hd(Result)),

   if (IsResultList) -> 
      Result;
      (1==1) ->
      [Result]
   end.




createSubLists(Matrx,RowNumber,ColNumber,MaxCol,MaxRow,CurrentI,CurrentJ,ArraysDone,FullResult,AddedResult,CurrentILoop) ->
    
    if
         (MaxRow =< CurrentI) ->
            if (AddedResult == 0) -> 
                  FinalResult = ArraysDone;
               (1==1) -> 
                     LastArrayIsList = is_list(hd(ArraysDone)),
                     if (LastArrayIsList) ->
                         FinalResult = FullResult++ArraysDone;
                        (1==1) ->
                           FinalResult = FullResult++[ArraysDone]
                     end
            end,
            
            FinalResult;
         (1==1) -> % works as an 'else' branch

            if  (CurrentILoop < RowNumber) ->
                  if (CurrentILoop == 0) ->                   
                     NewResult = rowPartitioner(hd(Matrx),ColNumber,MaxCol);
                     
                     (1==1) ->
                     NewResult = mergeSameList(ArraysDone, rowPartitioner(hd(Matrx),ColNumber,MaxCol))
                     
                  end,
                  createSubLists(tl(Matrx),RowNumber,ColNumber,MaxCol,MaxRow,CurrentI+1,CurrentJ,NewResult,FullResult,AddedResult,CurrentILoop+1);
               (1==1) ->
                  
                  if (is_list(hd(ArraysDone))) ->
                        if (AddedResult == 0) -> 
                              FinalResult = ArraysDone;
                           (1==1) ->
                              FinalResult = FullResult++ArraysDone
                           end;
                     (1==1) ->
                        if (AddedResult == 0) -> 
                           FinalResult = [ArraysDone];
                        (1==1) ->
                           FinalResult = FullResult++[ArraysDone]
                        end
                  end,
                  
                  createSubLists(Matrx,RowNumber,ColNumber,MaxCol,MaxRow,CurrentI,CurrentJ,ArraysDone,FinalResult,AddedResult+1,0) end            
    end.


rowPartitioner(Row,ColNumber,MaxCol)  -> rowPartitioner(Row,ColNumber,MaxCol,0,[]).

rowPartitioner(Row,ColNumber,MaxCol,CurrentJ,Result) ->
      
      Variable = CurrentJ + ColNumber,
      if 
         (Variable =< MaxCol) ->
            TempResult = sublist(Row,ColNumber),
            NewRow = Row -- TempResult,
            if (CurrentJ == 0) ->
                  NewResult = [Result,TempResult];
               (1==1) ->
                  NewResult = Result++[TempResult]
            end,
            rowPartitioner(NewRow,ColNumber,MaxCol,CurrentJ+1,NewResult);
         (1==1) ->
            TempResult = Row, 
            NewResult = Result ++ TempResult,
            
            filter(fun(X) -> X /= [] end, NewResult)
   end.

mergeSameList(L1,L2) -> mergeSameList(L1,L2,[]).

mergeSameList(L1,L2,Merged) ->
   % io:format("The value is: ~p,~p    ~p.~n", [L1,L2,Merged]),
   Size = flatlength(L1),
   if (Size > 0) ->
         IsList = is_list(hd(L1)) and is_list(hd(L2)) ,
         IsListL1 = is_list(hd(L1)),
         IsListL2 = is_list(hd(L2)),
         if (IsList) ->
               mergeSameList(tl(L1),tl(L2),Merged ++ [append([hd(L1),hd(L2)])]);
            (1==1) ->
               if (IsListL1) ->
                     mergeSameList([],[],Merged ++ append([hd(L1),L2]));
                  (IsListL2) ->
                     mergeSameList([],[],Merged ++ append([L1,hd(L2)]));
                  (1==1) ->
                     mergeSameList([],[],Merged ++ [append([L1,L2])])
               end
         end;
      (1==1) ->
         Merged
   end.





