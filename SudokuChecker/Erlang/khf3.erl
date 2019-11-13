-module(khf3).
-author('vicsuka@gmail.com').
-vsn('2019-11-13').
-export([megoldase/2]).
-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1]).
-import(khf1,[feldarabolasa/2]).
%-compile(export_all).

%-spec khf3:megoldase(SSpec :: sspec(), SSol :: ssol()) -> B :: bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.

megoldase(Proposal, Solution)-> checkSol(Proposal,Solution).

checkSol(ProposalMx , Solution)->
    {SudokuSize,_} = ProposalMx,
    {_,ProposalMatrix} = ProposalMx,
    Fullsize = length(ProposalMatrix),
    Result = checkBasicRules(Solution, SudokuSize, Fullsize),
    Result == 1.


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
    