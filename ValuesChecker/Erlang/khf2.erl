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

    Allrows = feldarabolasa(ProposalMatrix, {1,Fullsize}),
    Allcols = feldarabolasa(ProposalMatrix, {Fullsize,1}),
    Allsubcells = feldarabolasa(ProposalMatrix, {SudokuSize,SudokuSize}),
    SimpleList = feldarabolasa(ProposalMatrix,{1,1}),

    RowConstraint = getRowNumbers(Allrows,FieldRow,Fullsize),
    % io:format("RowConstraint: ~p ~n", [RowConstraint]),
    ColConstraint = getColNumbers(Allcols,FieldCol,Fullsize),
    % io:format("ColConstraint: ~p ~n", [ColConstraint]),
    SubConstraint = getSubNumbers(Allsubcells,SudokuSize,FieldRow,FieldCol),
    % io:format("SubConstraint: ~p ~n", [SubConstraint]),
    ParityConstraints = checkParity(SimpleList,FieldRow,FieldCol,Fullsize),
    % io:format("ParityConstraints: ~p ~n", [ParityConstraints]),
    NumberConstraint = checkNumber(SimpleList,FieldRow,FieldCol,Fullsize),
    % io:format("NumberConstraint: ~p ~n", [NumberConstraint]),

    Possibilites = generateAllPossibleValues(Fullsize,1,[]),
    % io:format("Possibilites: ~p ~n", [Possibilites]),

    Size = length(NumberConstraint),
    if (Size > 0) ->
            RowConstraint1 = RowConstraint -- NumberConstraint,
            ColConstraint1 = ColConstraint -- NumberConstraint,
            SubConstraint1 = SubConstraint -- NumberConstraint,
            ParityConstraint1 = ParityConstraints -- NumberConstraint,

            NewResult11 = NumberConstraint -- RowConstraint1,
            NewResult22 = NewResult11 -- ColConstraint1,
            NewResult33 = NewResult22 -- SubConstraint1,
            NewResult44 = NewResult33 -- ParityConstraint1,
            NewResult44;
        (1==1) ->
            NewResult = Possibilites -- RowConstraint,
            NewResult2 = NewResult -- ColConstraint,
            NewResult3 = NewResult2 -- SubConstraint,
            NewResult4 = NewResult3 -- ParityConstraints,
            NewResult4
    end.



getRowNumbers(Matrix,RowNumber,Size) ->
    Allrows = Matrix,
    Myrow = lists:nth(RowNumber, Allrows),
    extractNumbersFromList(Myrow,[]).

getColNumbers(Matrix,ColNumber,Size) ->
    Allcols = Matrix,
    Mycol = lists:nth(ColNumber, Allcols),
    extractNumbersFromList(Mycol,[]).

getSubNumbers(Matrix,SudokuSize,RowN,ColN) ->
    Allmx = Matrix,
    SubmxNumber = ((RowN - 1) div SudokuSize) * SudokuSize + ((ColN - 1) div SudokuSize) + 1,
    Mymx = lists:nth(SubmxNumber, Allmx),
    extractNumbersFromList(Mymx,[]).


extractNumbersFromList([],Result) -> Result;
extractNumbersFromList([H|T],Result) ->
    Number = getNumberFromList(H),
    if (Number > 0) ->
            NewResult = Result ++ [getNumberFromList(H)],
            extractNumbersFromList(T,NewResult);
        (1==1) ->
            extractNumbersFromList(T,Result)
    end.
    
generateAllPossibleValues(Size, CurrentValue, Output) ->
    if (CurrentValue > Size) ->
            Output;
        (1==1) ->
            NewOut = Output ++ [CurrentValue],
            generateAllPossibleValues(Size, CurrentValue+1, NewOut)
    end.
    
generateEvenValues(Size, CurrentValue, Output) ->
    if (CurrentValue > Size) ->
            Output;
        (1==1) ->
            IsEven = even(CurrentValue),
            if ( IsEven ) ->
                NewOut = Output ++ [CurrentValue],
                generateEvenValues(Size, CurrentValue+1, NewOut);
            (1==1) ->
                generateEvenValues(Size, CurrentValue+1, Output)
            end
    end.

    
generateOddValues(Size, CurrentValue, Output) ->
    if (CurrentValue > Size) ->
            Output;
        (1==1) ->
            IsOdd = odd(CurrentValue),
            if ( IsOdd ) ->
                NewOut = Output ++ [CurrentValue],
                generateOddValues(Size, CurrentValue+1, NewOut);
            (1==1) ->
                generateOddValues(Size, CurrentValue+1, Output)
            end
    end.

generateExactValues(Size, Number, CurrentValue, Output) ->
    if (CurrentValue > Size) ->
            Output;
        (1==1) ->
            if ( Number == CurrentValue ) ->
                NewOut = Output ++ [CurrentValue],
                generateExactValues(Size, Number, CurrentValue+1, NewOut);                
            (1==1) ->
                generateExactValues(Size, Number, CurrentValue+1, Output)
            end
    end.

checkParity(Matrix,RowN,ColN,Size) ->
    SimpleProposalList = Matrix,
    Myfield =  lists:nth(((RowN-1) * Size + ColN) , SimpleProposalList),
    % io:format("Constraint field: ~p ~n", [Myfield]),
    [List|_] = Myfield,

    CheckEven = listFind(e,List),
    CheckOdd = listFind(o,List),

    if (CheckEven) ->
            if (CheckOdd) ->
                generateAllPossibleValues(Size, 1, []);
            (1==1) ->
                generateOddValues(Size,1,[])
            end;
        (1==1) ->
            if (CheckOdd) ->
                generateEvenValues(Size, 1, []);
            (1==1) ->
                []
            end
    end.

checkNumber(Matrix,RowN,ColN,Size) ->
    SimpleProposalList = Matrix,
    Myfield =  lists:nth(((RowN-1) * Size + ColN) , SimpleProposalList),
    [List|_] = Myfield,

    CheckNumber = checkListForNumber(List),
    if (CheckNumber) ->
        Number = getNumberFromList(List),
        % io:format("Number: ~p ~n", [Number]),
        generateExactValues(Size, Number, 1, []);
    (1==1) -> 
        []
    end.


checkListForNumber([]) -> false;
checkListForNumber([H|T]) ->
    if (H < 16) ->
        if (H > 0) ->
            true;
        (1==1) ->
            checkListForNumber(T)
        end;
    (1==1) ->
        checkListForNumber(T)
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

listFind(Element, List) ->
  lists:member(Element, List).

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).
