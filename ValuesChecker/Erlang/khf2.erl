-module(khf2).
-author('vicsuka@gmail.com').
-vsn('2019-11-17').
-export([ertekek/2]).
-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1,nth/2]).
-import(khf1,[feldarabolasa/2]).
% -type col() :: integer().
% -type row() :: integer().
% -type coords() :: {row(),col()}.
% -spec khf2:ertekek(SSpec :: sspec(), R_C :: coords()) -> Vals :: [integer()].
%% Egy érték pontosan akkor szerepel a Vals listában, ha teljesíti a
%% fenti Prolog specifikációban felsorolt (a), (b) és (c) feltételeket, ahol
%% Vals az SSpec specifikációval megadott Sudoku-feladvány R_C
%% koordinátájú mezőjében megengedett értékek listája.


ertekek(Proposal, Field)-> checkField(Proposal,Field).

checkField(Proposal,Field) ->
    {SudokuSize,_} = Proposal,
    {_,ProposalMatrix} = Proposal,
    Fullsize = length(ProposalMatrix),
    {FieldRow,_} = Field,
    {_,FieldCol} = Field,
    RowConstraint = getRowNumbers(ProposalMatrix,FieldRow,Fullsize),
    io:format("RowConstraint: ~p ~n", [RowConstraint]),
    ColConstraint = getColNumbers(ProposalMatrix,FieldCol,Fullsize),
    io:format("ColConstraint: ~p ~n", [ColConstraint]),
    % SubConstraint = getSubNumbers(SudokuSize,ProposalMatrix,FieldRow,FieldCol,Fullsize),
    io:format("LENGTH: ~n").

getRowNumbers(Matrix,RowNumber,Size) ->
    Allrows = feldarabolasa(Matrix, {1,Size}),
    Myrow = lists:nth(RowNumber, Allrows),
    extractNumbersFromList(Myrow,[]).

getColNumbers(Matrix,ColNumber,Size) ->
    Allcols = feldarabolasa(Matrix, {Size,1}),
    Mycol = lists:nth(ColNumber, Allcols),
    extractNumbersFromList(Mycol,[]).

extractNumbersFromList([],Result) -> Result;
extractNumbersFromList([H|T],Result) ->
    Number = getNumberFromList(H),
    if (Number > 0) ->
            NewResult = Result ++ [getNumberFromList(H)],
            extractNumbersFromList(T,NewResult);
        (1==1) ->
            extractNumbersFromList(T,Result)
    end.
    

getNumberFromList([]) -> 0;
getNumberFromList([H|T]) ->
    if (H < 16 ) ->
        if (H > 0 ) ->
            H;
        (1==1) ->
            getNumberFromList(T)
        end;
    (1==1) ->
        getNumberFromList(T)
    end.

