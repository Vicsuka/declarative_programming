-module(khf3).
-author('vicsuka@gmail.com').
-vsn('2019-11-13').
-export([megoldase/2]).
-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1,nth/2]).
-import(khf1,[feldarabolasa/2]).
%-compile(export_all).

%-spec khf3:megoldase(SSpec :: sspec(), SSol :: ssol()) -> B :: bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.

megoldase(Proposal, Solution)-> checkSol(Proposal,Solution).

checkSol(ProposalMx , Solution)->
    {SudokuSize,_} = ProposalMx,
    {_,ProposalMatrix} = ProposalMx,
    Fullsize = length(Solution),

    ResultBasic = checkBasicRules(Solution, SudokuSize, Fullsize),

    SimpleProposalList = feldarabolasa(ProposalMatrix,{1,1}),
    SimpleSolutionList = feldarabolasa(Solution,{1,1}),
    ResultParity = checkOddEven(SimpleProposalList,SimpleSolutionList),

    ResultNeighb = checkNeighbors(SimpleProposalList,SimpleSolutionList,[],Fullsize),

    ResultNumbers = checkNumbers(SimpleProposalList,SimpleSolutionList),

    if (ResultParity == 1) ->
        if (ResultBasic == 1) ->
           if (ResultNeighb == 1) ->
                if (ResultNumbers == 1) ->
                    1==1;
                (1==1) ->
                    1==0
                end;
            (1==1) ->
                1==0
            end;
        (1==1) ->
            1==0
        end;
    (1==1) ->
        1==0
    end.

checkNeighbors([],[],_,_) -> 1;
checkNeighbors(_,[],_,_) -> 1;
checkNeighbors([],_,_,_) -> 1;

checkNeighbors([[PropHead|_]|PropTail],[SolHead|SolTail],[],SeekSize) ->
    TailLength = length(SolTail),
    % io:format("LENGTH: ~p ~n", [TailLength]),
    if (TailLength >= SeekSize) ->
            [SouthElement|_] = nth(SeekSize, SolTail);
        (1==1) -> 
            SouthElement = 0
    end,

    % io:format("Value: ~p ~n", [SouthElement]),

    CheckWest = listFind(w,PropHead),
    if (CheckWest) ->
        ProceedWest = false;
    (1==1) -> 
        ProceedWest = true
    end,

    CheckSouth = listFind(s,PropHead),
    if (CheckSouth) ->
        ProceedSouth = odd(hd(SolHead)+SouthElement);
    (1==1) -> 
        ProceedSouth = true
    end,


    if (ProceedWest and ProceedSouth) ->
        checkNeighbors(PropTail,SolTail,SolHead,SeekSize);
    (1==1) ->
        0
    end;

checkNeighbors([[PropHead|_]|PropTail],[SolHead|SolTail],[Previous|_],SeekSize) ->
    TailLength = length(SolTail),
    % io:format("LENGTH: ~p ~n", [TailLength]),
    if (TailLength >= SeekSize) ->
            [SouthElement|_] = nth(SeekSize, SolTail);
        (1==1) -> 
            SouthElement = 0
    end,

    % io:format("Value: ~p ~n", [SouthElement]),

    CheckWest = listFind(w,PropHead),
    if (CheckWest) ->
        ProceedWest = odd(hd(SolHead)+Previous);
    (1==1) -> 
        ProceedWest = true
    end,

    CheckSouth = listFind(s,PropHead),
    if (CheckSouth) ->
        
        ProceedSouth = odd(hd(SolHead)+SouthElement);
    (1==1) -> 
        ProceedSouth = true
    end,


    if (ProceedWest and ProceedSouth) ->
        checkNeighbors(PropTail,SolTail,SolHead,SeekSize);
    (1==1) ->
        0
    end.


checkOddEven([],[]) -> 1;
checkOddEven(_,[]) -> 1;
checkOddEven([],_) -> 1;
checkOddEven([[PropHead|_]|PropTail],[SolHead|SolTail]) ->

    CheckEven = listFind(e,PropHead),
    if (CheckEven) ->
        ProceedEven = even(hd(SolHead));
    (1==1) -> 
        ProceedEven = true
    end,

    CheckOdd = listFind(o,PropHead),
    if (CheckOdd) ->
        ProceedOdd = odd(hd(SolHead));
    (1==1) -> 
        ProceedOdd = true
    end,

    if (ProceedEven and ProceedOdd) ->
        checkOddEven(PropTail,SolTail);
    (1==1) ->
        0
    end.

checkNumbers([],[]) -> 1;
checkNumbers(_,[]) -> 1;
checkNumbers([],_) -> 1;
checkNumbers([[PropHead|_]|PropTail],[[SolHead|_]|SolTail]) ->

    CheckNumber = checkListForNumber(PropHead),
    if (CheckNumber) ->
        RequestedNumber = getNumberFromList(PropHead),
        % io:format("Value: ~p vs ~p ~n", [RequestedNumber,SolHead]),
        ProceedNumber = SolHead==RequestedNumber;
    (1==1) -> 
        ProceedNumber = true
    end,

    if (ProceedNumber) ->
        checkNumbers(PropTail,SolTail);
    (1==1) ->
        0
    end.


checkBasicRules(Solution, SudokuSize, FullSize) ->
    SolutionCols = feldarabolasa(Solution, {1,FullSize}),
    SolutionRows = feldarabolasa(Solution, {FullSize,1}),
    SolutionSubMatrixes = feldarabolasa(Solution, {SudokuSize,SudokuSize}),
    FullList = append([SolutionCols,SolutionRows,SolutionSubMatrixes]),
    checkDuplicate(FullList).
    % io:format("Full list: ~p ~n", [FullList]).
    


checkDuplicate([]) -> 1;
checkDuplicate([H|T]) ->
    SizeList = length(H),
    SizeSet = sets:size(sets:from_list(H)),
    if (SizeList == SizeSet) ->
        checkDuplicate(T);
    (1==1) -> 
        % io:format("Duplicate in: ~p ~n", [H]),
        0
    end.
    

listFind(Element, List) ->
  lists:member(Element, List).

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).

checkListForNumber([]) -> false;
checkListForNumber([H|T]) ->
    if (H < 101 ) ->
        if (H > 0 ) ->
            true;
        (1==1) ->
            checkListForNumber(T)
        end;
    (1==1) ->
        checkListForNumber(T)
    end.


getNumberFromList([]) -> 0;
getNumberFromList([H|T]) ->
    if (H < 101 ) ->
        if (H > 0 ) ->
            % io:format("RETURNING: ~p ~n", [H]),
            H;
        (1==1) ->
            getNumberFromList(T)
        end;
    (1==1) ->
        getNumberFromList(T)
    end.