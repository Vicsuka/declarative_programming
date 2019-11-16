% :- pred megoldase(sspec::in, ssol::in).
% megoldase(+SSpec,+SSol) : sikeres, ha az SSol érték-mátrix megoldása az SSpec Sudoku-feladványnak.
:- use_module(library(lists)).

megoldase(Proposal, Solution):- checkSol(Proposal, Solution).

checkSol(s(SudokuSize,[PropHead|PropTail]) , [SolHead|SolTail]):-
    % writeln('Requested: '),
    % writeln(SudokuSize),
    % writeln(PropHead),
    % writeln(PropTail),
    % writeln(SolHead),
    % writeln(SolTail),

    length([SolHead|SolTail],Fullsize),
    % writeln(Fullsize),

    checkBasicRules([SolHead|SolTail], SudokuSize, Fullsize, ResultBasic).

    feldarabolasa([PropHead|PropTail],1-1,SimpleProposalList),
    feldarabolasa([SolHead|SolTail],1-1,SimpleSolutionList),

    % checkOddEven(SimpleProposalList,SimpleSolutionList,ResultParity),

    % checkNeighbors(SimpleProposalList,SimpleSolutionList,[],Fullsize,ResultNeighb),

    % checkNumbers(SimpleProposalList,SimpleSolutionList,ResultNumbers).












checkBasicRules([Head|Tail], SudokuSize, FullSize, Result) :-
    feldarabolasa([Head|Tail], 1-FullSize, SolutionCols),
    feldarabolasa([Head|Tail], FullSize-1, SolutionRows),
    feldarabolasa([Head|Tail], SudokuSize-SudokuSize, SolutionSubMatrixes),
    append3(SolutionCols,SolutionRows,SolutionSubMatrixes,FullList),
    writeln(FullList),
    checkDuplicate(FullList).


checkDuplicate([]) :- true.
checkDuplicate([H|T]) :-
    
    (is_set(H)) ->
        checkDuplicate(T);
    (
        writeln('Duplicate in: '),
        writeln(H),
        false
    ).
        

is_set(Lst) :-
    setof(X, member(X, Lst), Set),
    length(Lst, N),
    length(Set, N).

append3(X, Y, Z, XYZ) :-
   append(X, YZ, XYZ),
   append(Y, Z, YZ).

















feldarabolasa([FirstRow|Rest], RowNumber-ColNumber, LL):-

    length([FirstRow|Rest],MaxRow),
    length(FirstRow, MaxCol),


    createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,0,0,[],[],0,0,Result),
    getHead(Result,V),


    (is_list(V) -> append(Result,[],LL);
     append([Result],[],LL)
    ),

    !.



retrieveData(P,L):- split_string(P,"-","",L).

getHead([H|_],V) :- V= H.

createSubLists([],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],[FullResultHead|FullResultRest],AddedResult,CurrentILoop,Result) :-
    (
        AddedResult =:= 0 -> append([DoneArrayHead|DoneArrayRest],[],Finito);
        (
            is_list(DoneArrayHead) -> append([[FullResultHead|FullResultRest],[DoneArrayHead|DoneArrayRest]],Finito);
            append([[FullResultHead|FullResultRest],[[DoneArrayHead|DoneArrayRest]]],Finito)

        ) 
    ),
    append(Finito,[],Result).

createSubLists([],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],[],AddedResult,CurrentILoop,Result) :-
    append([DoneArrayHead|DoneArrayRest],[],Result).

createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],[],AddedResult,CurrentILoop,Result) :-
    (
        MaxRow < CurrentI+1 ->
            (
                AddedResult =:= 0 ->
                            rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,TmpNewResult),
                            mergeSameList([DoneArrayHead|DoneArrayRest], TmpNewResult, Merge,NewResult),
                            append(NewResult,[],Finito);
                (
                    is_list(DoneArrayHead) -> append([[],[DoneArrayHead|DoneArrayRest]],Finito);
                    append([[],[[DoneArrayHead|DoneArrayRest]]],Finito)
                ) 
            ),
            append(Finito,[],Result);
        (
            (CurrentILoop < RowNumber) ->
                (
                    CurrentILoop =:= 0 -> rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,NewResult);
                    rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,TmpNewResult), 
                    mergeSameList([DoneArrayHead|DoneArrayRest], TmpNewResult, Merge,NewResult)
                ),
                createSubLists(Rest,RowNumber,ColNumber,MaxRow,MaxCol,CurrentI+1,CurrentJ,NewResult,[],AddedResult,CurrentILoop+1,Result);
            (
                is_list(DoneArrayHead) -> 
                    (
                        AddedResult =:= 0 -> append([DoneArrayHead|DoneArrayRest],[],Finito)  ;
                        append([[],[DoneArrayHead|DoneArrayRest]],Finito)
                    );
                (
                    AddedResult =:= 0 -> append([[DoneArrayHead|DoneArrayRest]],[],Finito);
                    append([[],[[DoneArrayHead|DoneArrayRest]]],Finito)
                )
            ),
            createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],Finito,AddedResult+1,0,Result)                             
        )
    ).



createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[],[],AddedResult,CurrentILoop,Result) :-
    rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,NewResult),
    (
        MaxRow =< CurrentI -> append(NewResult,[],Result);
        createSubLists(Rest,RowNumber,ColNumber,MaxRow,MaxCol,CurrentI+1,CurrentJ,NewResult,[],AddedResult,CurrentILoop+1,Result)
    ).



createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],[FullResultHead|FullResultRest],AddedResult,CurrentILoop,Result) :-
    (
        MaxRow+1 < CurrentI ->
            (
                AddedResult =:= 0 -> append([DoneArrayHead|DoneArrayRest],[],Finito);
                (
                    is_list(DoneArrayHead) -> append([[FullResultHead|FullResultRest],[DoneArrayHead|DoneArrayRest]],Finito);
                    append([[FullResultHead|FullResultRest],[[DoneArrayHead|DoneArrayRest]]],Finito)

                ) 
            ),
            append(Finito,[],Result);
        (
            (CurrentILoop < RowNumber) ->
                (
                    CurrentILoop =:= 0 -> rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,NewResult);
                    rowPartitioner(FirstRow,ColNumber,MaxCol,0,Finally,TmpNewResult), 
                    mergeSameList([DoneArrayHead|DoneArrayRest], TmpNewResult, Merge, NewResult)
                ),
                createSubLists(Rest,RowNumber,ColNumber,MaxRow,MaxCol,CurrentI+1,CurrentJ,NewResult,[FullResultHead|FullResultRest],AddedResult,CurrentILoop+1,Result);
            (
                is_list(DoneArrayHead) -> 
                    (
                        AddedResult =:= 0 -> append([DoneArrayHead|DoneArrayRest],[],Finito)  ;
                        append([[FullResultHead|FullResultRest],[DoneArrayHead|DoneArrayRest]],Finito)
                    );
                (
                    AddedResult =:= 0 -> append([[DoneArrayHead|DoneArrayRest]],[],Finito);
                    append([[FullResultHead|FullResultRest],[[DoneArrayHead|DoneArrayRest]]],Finito)
                )
            ),
            createSubLists([FirstRow|Rest],RowNumber,ColNumber,MaxRow,MaxCol,CurrentI,CurrentJ,[DoneArrayHead|DoneArrayRest],Finito,AddedResult+1,0,Result)                             
        )
    ).

subList(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

trim( []    , 0 , []    ) .  % Trimming zero elements from the empty list yields the empty list
trim( [H|T] , 0 , [H|T] ) .  % Trimming zero elements from a non-empty list yields the same list  
trim( [H|T] , N , R     ) :- % Otherwise,
  N > 0 ,                    % - given that N is greater than zero
  N1 is N-1 ,                % - we decrement N
  trim( T , N1 , R )         % - and recurse down, discarding the head of the list.
  .                          % That's about all there is to it.

rowPartitioner(Row,ColNumber,MaxCol,CurrentJ,[ResultHead|ResultRest],Finally) :-
      length(Row,Variable),
      (
          (ColNumber =< Variable) -> subList(Row,ColNumber,TempResult),
                                  trim(Row,ColNumber,NewRow),
                                  (
                                      CurrentJ =:= 0 -> append([],[TempResult],NewResult);
                                      append([ResultHead|ResultRest],[TempResult],NewResult)
                                  ),
                                  rowPartitioner(NewRow,ColNumber,MaxCol,CurrentJ+1,NewResult,Finally);
            length(Row,Size),
            
            (
                Size > 0 -> 
                    (
                        CurrentJ =:= 0 -> append([Row],[],Finally);
                        append([ResultHead|ResultRest], [Row],Finally)
                    );
                    
                append([ResultHead|ResultRest],[],Finally)
            )

      ).

mergeSameList([],[],Merged,Exit) :-
    append(Merged,[],Exit).

mergeSameList([L1Head|L1Tail],[L2Head|L2Tail],Merged,Exit) :-
    length([L1Head|L1Tail], Size),
    (
        Size > 0 ->
            (
                is_list(L1Head) ->
                    (
                        is_list(L2Head) -> append(L1Head,L2Head,OnlyHeads),
                                           append(Merged,[OnlyHeads],Finito),
                                           mergeSameList(L1Tail,L2Tail,Finito,Exit);
                        append(L1Head,[L2Head|L2Tail],OnlyHeads),
                        append(Merged,OnlyHeads,Finito),
                        mergeSameList([],[],Finito,Exit)
                    );
                is_list(L2Head) ->  append([L1Head|L1Tail],L2Head,OnlyHeads),
                                    append(Merged,OnlyHeads,Finito),
                                    mergeSameList([],[],Finito,Exit);
                append([L1Head|L1Tail],[L2Head|L2Tail],OnlyHeads),
                append(Merged,OnlyHeads,Finito),
                mergeSameList([],[],Finito,Exit)
            );
        append(Merged,[],Exit)
    ).
