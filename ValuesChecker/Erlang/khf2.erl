-module(khf2).
-author('vicsuka@gmail.com').
-vsn('2019-11-17').
-export([ertekek/6]).
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


ertekek(Proposal, Field,Allrows,Allcols,Allsubcells,SimpleList)-> checkField(Proposal,Field,Allrows,Allcols,Allsubcells,SimpleList).

checkField(Proposal,Field,Allrows,Allcols,Allsubcells,SimpleList) ->
    {SudokuSize,_} = Proposal,
    {_,ProposalMatrix} = Proposal,
    Fullsize = length(ProposalMatrix),
    {FieldRow,_} = Field,
    {_,FieldCol} = Field,

    RowConstraint = getRowNumbers(Allrows,FieldRow,Fullsize),
    % io:format("RowConstraint: ~p ~n", [RowConstraint]),
    ColConstraint = getColNumbers(Allcols,FieldCol,Fullsize),
    % io:format("ColConstraint: ~p ~n", [ColConstraint]),
    SubConstraint = getSubNumbers(Allsubcells,SudokuSize,FieldRow,FieldCol),
    % io:format("SubConstraint: ~p ~n", [SubConstraint]),
    ParityConstraints = checkParity(SimpleList,FieldRow,FieldCol,Fullsize),
    % io:format("ParityConstraints: ~p ~n", [ParityConstraints]),

    NeighBorConstraints = checkNeighborsCONSTRAINT(SimpleList,FieldRow,FieldCol,Fullsize),
    % io:format("NeighBorConstraints: ~p ~n", [NeighBorConstraints]),

    % {2, [[   [2],          [],         [e],          []],
            % [    [],         [s],          [],         [o]],
            % [    [],          [],         [1],          []],
            % [    [],         [w],          [],          []]]}, {1,4}).

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
            NeighBorConstraints1 = NeighBorConstraints -- NumberConstraint,

            NewResult11 = NumberConstraint -- RowConstraint1,
            NewResult22 = NewResult11 -- ColConstraint1,
            NewResult33 = NewResult22 -- SubConstraint1,
            NewResult44 = NewResult33 -- ParityConstraint1,
            NewResult55 = NewResult44 -- NeighBorConstraints1,
            NewResult55;
        (1==1) ->
            NewResult = Possibilites -- RowConstraint,
            NewResult2 = NewResult -- ColConstraint,
            NewResult3 = NewResult2 -- SubConstraint,
            NewResult4 = NewResult3 -- ParityConstraints,
            NewResult5 = NewResult4 -- NeighBorConstraints,
            NewResult5
    end.

checkNeighborsCONSTRAINT(Matrix,RowN,ColN,Size) ->
    SimpleProposalList = Matrix,
    Myfield =  lists:nth(((RowN-1) * Size + ColN) , SimpleProposalList),


    [List|_] = Myfield,

    CheckSouth = listFind(s,List),
    CheckWest = listFind(w,List),

    if (CheckSouth) ->
        if (RowN == Size) ->
            generateAllPossibleValues(Size, 1, []);
        (1==1) ->
            if (CheckWest) ->
                if (ColN == 1) ->
                    generateAllPossibleValues(Size, 1, []);
                (1==1) ->
                    %CHECK BOTH
                    WestField = lists:nth(((RowN-1) * Size + ColN - 1) , SimpleProposalList),
                    SouthField = lists:nth(((RowN-1) * Size + ColN + Size) , SimpleProposalList),

                    [WestList|_] = WestField,
                    [SouthList|_] = SouthField,

                    WestNumberC = checkListForNumber(WestField),
                    SouthNumberC = checkListForNumber(SouthList),

                    if (SouthNumberC) ->
                        SouthNumber = getNumberFromList(SouthList),
                        if (WestNumberC) ->
                            WestNumber = getNumberFromList(WestList),
                            %CHECK BOTH NUMBER
                            IsSouthEven = even(SouthNumber),
                            IsWestEven = even(WestNumber),
                            if (IsSouthEven) ->
                                if (IsWestEven) ->
                                    generateEvenValues(Size,1,[]);
                                (1==1) ->
                                    generateAllPossibleValues(Size, 1, []) 
                                end;  
                            (1==1) ->
                                if (IsWestEven) ->
                                    generateAllPossibleValues(Size,1,[]);
                                (1==1) ->
                                    generateOddValues(Size, 1, [])  
                                end 
                            end;
                        (1==1) ->
                            %SOUTH NUMBER
                            IsSouthEven = even(SouthNumber),
                            if (IsSouthEven) ->
                                generateEvenValues(Size,1,[]);
                            (1==1) ->
                                generateOddValues(Size,1,[])  
                            end                            
                        end;   
                    (1==1) -> 
                        if (WestNumberC) ->
                            WestNumber = getNumberFromList(WestList),
                            %WEST NUMBER
                            IsWestEven = even(WestNumber),
                            if (IsWestEven) ->
                                generateEvenValues(Size,1,[]);
                            (1==1) ->
                                generateOddValues(Size,1,[])  
                            end;  
                        (1==1) ->
                            []
                        end
                    end

                end;
            (1==1) ->
                %CHECK SOUTH
                SouthField = lists:nth(((RowN-1) * Size + ColN + Size) , SimpleProposalList),
                [SouthList|_] = SouthField,
                SouthNumberC = checkListForNumber(SouthList),

                if (SouthNumberC) ->
                    SouthNumber = getNumberFromList(SouthList),
                    %SOUTH NUMBER
                    IsSouthEven = even(SouthNumber),
                    if (IsSouthEven) ->
                        generateEvenValues(Size,1,[]);
                    (1==1) ->
                        generateOddValues(Size,1,[])  
                    end;                              
                (1==1) ->
                    []
                end
            end
        end;            
    (1==1) ->
        if (CheckWest) ->
            if (ColN == 1) ->
                generateAllPossibleValues(Size, 1, []);
            (1==1) ->
                %CHECK WEST
                % WestField = lists:nth(((RowN-1) * Size + ColN - 1) , SimpleProposalList),
                % [WestList|_] = WestField,
                % WestNumberC = checkListForNumber(WestList),

                % if (WestNumberC) ->
                %     WestNumber = getNumberFromList(WestList),
                %     %WEST NUMBER
                %     IsWestEven = even(WestNumber),
                %     if (IsWestEven) ->
                %         generateEvenValues(Size,1,[]);
                %     (1==1) ->
                %         generateOddValues(Size,1,[])  
                %     end;                              
                % (1==1) ->
                    []
                % end
            end;
        (1==1) ->
            []
        end
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
    if (H < 101) ->
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
    if (H < 101 ) ->
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
