-module(sudoku).
-author('vicsuka@gmail.com').
-vsn('2019-11-17').
-export([sudoku/1,addFirstPossibleValue/7,addPossibleSteps/3,removeDoubleBrackets/2,multiplyList/4,createPartition/5]).

-import(khf1,[feldarabolasa/2]).
-import(khf2,[ertekek/6]).
-import(khf3,[megoldase/2]).

-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,append/2,flatlength/1]).
-import(math,[sqrt/1]).

% -type sspec() :: {size(), board()}.
% -type size()  :: integer().
% -type field() :: [info()].
% -type info()  :: e | o | s | w | integer().
% -type board() :: [[field()]].

% -type ssol() :: [[integer()]].

% -spec sudoku:sudoku(SSpec :: sspec()) -> SSols :: [ssol()].
%% SSols az SSpec feladványt kielégítő megoldások listája.
sudoku({_,Matrix}) -> solveProblems(Matrix, []).

solveProblems(Matrix, []) ->
    
    Fullsize = length(Matrix),
    ReSolution = solveSudokuV3(Fullsize,Matrix,[],[],1,[]),
    Done = createPartition(ReSolution, Fullsize,[],[],1),
    Done.

solveSudokuV3(Fullsize,Matrix,[],[],Counter,[]) ->
    SimpleMatrixList = feldarabolasa(Matrix,{1,1}),

    Rows = feldarabolasa(Matrix, {1,Fullsize}),
    Cols = feldarabolasa(Matrix, {Fullsize,1}),
    Cells = feldarabolasa(Matrix, {isqrt(Fullsize),isqrt(Fullsize)}),
    FullLists = feldarabolasa(Matrix,{1,1}),

    PossibleSteps = calculatePossibilites(isqrt(Fullsize),Matrix,SimpleMatrixList,1,1,[],Rows,Cols,Cells,FullLists),
    % SimpleList = addPossibleSteps(SimpleMatrixList, [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],[]),
    % %io:format("Possible: ~p ~n",[PossibleSteps]),
    NoBrackets = removeDoubleBrackets(SimpleMatrixList,[]),

    solveSudokuV3(Fullsize,Matrix,PossibleSteps,NoBrackets,Counter,[]);


solveSudokuV3(Fullsize,Matrix,PossibleSteps,SimpleMatrixList,Counter,Solution) ->    
    if (Counter > 1000) ->
        io:format("COULD NOT SOLVE ~n");
    (1==1) ->           
        % io:format("STEP: ~p ~n",[Counter]),
        % io:format("FOR MATRIX: ~p ~n",[Matrix]),
        % io:format("Possible: ~p ~n",[PossibleSteps]),
        % io:format("List: ~p ~n",[SimpleMatrixList]),

        %ADD VALUES THAT HAVE ONLY 1 POSSIBILITY
        NextSimpleList = addPossibleSteps(SimpleMatrixList,PossibleSteps,[]),
        % io:format("SURE VALUES: ~p ~n",[NextSimpleList]),

        if (NextSimpleList == SimpleMatrixList) ->

            %FIND BEST FIELD (SMALLEST POSSIBLE STEPS BIGGER THAN 1)
            Smallest = findSmallest(PossibleSteps,16),
            %io:format("Smallest: ~p ~n",[Smallest]),
            if (Smallest > 15) ->
                {Fullsize,Matrix};
            (1==1) ->


                %ADD FIRST POSSIBLE VALUE TO MX
                ToReplaceList = tryDifferentValues(SimpleMatrixList, PossibleSteps, [], Smallest),
                
                %io:format("WE CHOSE: ~p, BACKUPS: ~p at: ~p ~n",[AdvancedSimpleList,NewBackUpArray,NewBackUpIndex]),
                AllPossibleMatrixes = multiplyList(ToReplaceList,Smallest,1,[]),
                BranchingArray = getBranch(PossibleSteps,Smallest),
                
                AdvancedSimpleLists = forAllList(AllPossibleMatrixes, BranchingArray, []),
                % io:format("AdvancedSimpleLists: ~p ~n",[AdvancedSimpleLists]),


                SolutionArray = doBranching(Fullsize,AdvancedSimpleLists,[],Counter),
                % io:format("SolutionArray SIZE: ~p ~n",[length(SolutionArray)]),
                % io:format("SolutionArray: ~p ~n",[SolutionArray]),
                SolutionArray
            end;
            
        (1==1) ->
            % io:format("ADDED VALUE%!!!% ~n"),
            %PARTITION CURRENT CONSTRAINT TO MATRIX
            NextMatrix = createPartition(NextSimpleList,Fullsize,[],[],1),
            %io:format("NextMatrix: ~p ~n",[NextMatrix]),

            Rows = feldarabolasa(NextMatrix, {1,Fullsize}),
            Cols = feldarabolasa(NextMatrix, {Fullsize,1}),
            Cells = feldarabolasa(NextMatrix, {isqrt(Fullsize),isqrt(Fullsize)}),
            FullLists = feldarabolasa(NextMatrix,{1,1}),
            %NEXT POSSIBLE MOVES
            NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,NextSimpleList,1,1,[],Rows,Cols,Cells,FullLists),
            %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

            %CURRENT SOLUTION FOR CHECKING
            ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
            PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
            %io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

            %LOGGING
            % io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

            %CHECK IF SUDOKU IS SOLVEABLE
            Possible = checkPossibility(NextPossibleSteps),
            %io:format("Possible? ~p ~n",[Possible]),

            IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
            % io:format("DONE? ~p ~n",[IsDone]),

            if (IsDone) ->
                % io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),
                PartitionedSolution;
            (1==1) ->
                if (Possible) ->
                    solveSudokuV3(Fullsize,NextMatrix,NextPossibleSteps,NextSimpleList,Counter+1,Solution);
                (1==1) ->                
                    {Fullsize,Matrix}
                end
            end
        
        end
        
    end.

% partitionByFullSize([],_, Res, _) ->Res;
% partitionByFullSize([H|T],Size, Res, Tmp) ->
%     [].

% createPartition([],_,_,Result,_) -> Result;
% createPartition([H|T],Size,TempResult,Result,Counter) ->
%     if (Counter == Size) ->
%         NewRes = TempResult++[H],
%         NewFullRes = Result++[NewRes],
%         createPartition(T,Size,[],NewFullRes,1);
%     (1==1) ->
%         NewRes = TempResult++[H],
%         createPartition(T,Size,NewRes,Result,Counter+1)
%     end.


doBranching(_,[],Results,_) -> Results;
doBranching(Fullsize,[CurrentList|NextList], Results,Counter) ->
    %PARTITION CURRENT CONSTRAINT TO MATRIX
    NextMatrix = createPartition(CurrentList,Fullsize,[],[],1),
    %io:format("NextMatrix: ~p ~n",[NextMatrix]),

    %NEXT POSSIBLE MOVES
    Rows = feldarabolasa(NextMatrix, {1,Fullsize}),
    Cols = feldarabolasa(NextMatrix, {Fullsize,1}),
    Cells = feldarabolasa(NextMatrix, {isqrt(Fullsize),isqrt(Fullsize)}),
    FullLists = feldarabolasa(NextMatrix,{1,1}),
    NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,CurrentList,1,1,[],Rows,Cols,Cells,FullLists),
    %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

    %CURRENT SOLUTION FOR CHECKING
    ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
    PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
    % io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

    %LOGGING
    % io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

    %CHECK IF SUDOKU IS SOLVEABLE
    Possible = checkPossibility(NextPossibleSteps),

    IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
    % io:format("DONE? ~p ~n",[IsDone]),

    if (IsDone) ->
        NewResults = Results ++ PartitionedSolution,
        doBranching(Fullsize, NextList, NewResults,Counter);
    (1==1) ->
        if (Possible) ->
            Solve = solveSudokuV3(Fullsize,NextMatrix,NextPossibleSteps,CurrentList,Counter+1,Results),
            if (is_list(Solve)) ->
                % io:format("Solve: ~p ~n",[Solve]),
                ResponseSize = length(Solve),
                % io:format("ResponseSize: ~p ~n",[ResponseSize]),
                if (ResponseSize > 1) ->
                    NewResults = Results ++ Solve,
                    doBranching(Fullsize, NextList, NewResults,Counter);
                (1==1) ->
                    doBranching(Fullsize, NextList, Results,Counter)
                end;
            (1==1) ->
                doBranching(Fullsize, NextList, Results,Counter)
            end;
        (1==1) ->                
            {Fullsize,NextMatrix}
        end
    end.


multiplyList([], _, _, FullList) -> FullList;
multiplyList([H|T], Size, Counter, FullList) ->
    if (Size < Counter) ->
        multiplyList([],Size,Counter,FullList);
    (1==1) ->
        NewRes = FullList ++ [[H|T]],
        multiplyList([H|T], Size, Counter+1, NewRes)
    end.

getBranch([PossH|PossT],Smallest) ->
    PossibNumber = length(PossH),
    if (PossibNumber == Smallest) ->
        PossH;
    (1==1) ->
        getBranch(PossT, Smallest)
    end.

forAllList([],[],Result) -> Result;
forAllList([],_,Result) -> Result;
forAllList(_,[],Result) -> Result;
forAllList([H|T], [VH|VT], Result) ->
    NewVal = Result ++ [replaceValues(H,VH,[])],
    forAllList(T,VT,NewVal).

replaceValues([],[],Result) -> Result;
replaceValues([],_,Result) -> Result;
replaceValues(_,[],Result) -> Result;
replaceValues([SolH|SolT], Value, Result) ->
    Contained = listFind(replace,SolH),
    if (Contained) ->
        %Remove last element that is 'replace'
        Appended = append(lists:reverse(tl(lists:reverse(SolH))),[Value]),
        NewRes = Result ++ [Appended],
        replaceValues(SolT,Value,NewRes);
    (1==1) ->
        NewRes = Result ++ [SolH],
        replaceValues(SolT,Value,NewRes)
    end.

tryDifferentValues([],[],Result, _) -> Result;
tryDifferentValues([],_,Result, _) -> Result;
tryDifferentValues(_,[],Result, _) -> Result;
tryDifferentValues([SolH|SolT], [PossH|PossT], Result, Smallest) ->
    PossibNumber = length(PossH),
    if (PossibNumber == Smallest) ->
            [TheValue|_] = PossH,
            AlreadyContained = listFind(TheValue,SolH),
            if (AlreadyContained) ->
                NewRes = Result ++ [SolH],
                tryDifferentValues(SolT,PossT,NewRes, Smallest);
            (1==1) ->
                Appended = append(SolH,[replace]),
                NewRes = Result ++ [Appended],
                tryDifferentValues(SolT,PossT,NewRes, 999)
            end;
    (1==1) ->
        NewRes = Result ++ [SolH],
        tryDifferentValues(SolT,PossT,NewRes, Smallest)
    end.

removeDoubleBrackets([],Result) ->Result;
removeDoubleBrackets([[H|_]|T],Result) ->
    NewRes = Result ++ [H],
    removeDoubleBrackets(T,NewRes).

addFirstPossibleValue([],[],Result, BackArr, _, Index, _) -> {Result,BackArr,Index};
addFirstPossibleValue([],_,Result, BackArr, _, Index, _) -> {Result,BackArr,Index};
addFirstPossibleValue(_,[],Result, BackArr, _, Index, _) -> {Result,BackArr,Index};
addFirstPossibleValue([SolH|SolT], [PossH|PossT], Result, BackArr, Smallest, Index, MaxLength) ->
    PossibNumber = length(PossH),
    if (PossibNumber == Smallest) ->
            [TheValue|NewBackup] = PossH,
            AlreadyContained = listFind(TheValue,SolH),
            if (AlreadyContained) ->
                NewRes = Result ++ [SolH],
                addFirstPossibleValue(SolT,PossT,NewRes, BackArr, Smallest, Index, MaxLength+1);
            (1==1) ->
                Appended = append(SolH,[TheValue]),
                NewRes = Result ++ [Appended],
                addFirstPossibleValue(SolT,PossT,NewRes, NewBackup, 999, MaxLength, MaxLength+1)
            end;
    (1==1) ->
        NewRes = Result ++ [SolH],
        addFirstPossibleValue(SolT,PossT,NewRes, BackArr, Smallest, Index, MaxLength+1)
    end.


findSmallest([],Smallest) -> Smallest;
findSmallest([H|T],Smallest) ->
    Size = length(H),
    if ( Size < Smallest ) ->
        if ( Size > 1 ) ->
            NewSmallest = Size,
            findSmallest(T,NewSmallest);
        (1==1) ->
            findSmallest(T,Smallest)
        end;
    (1==1) ->
        findSmallest(T,Smallest)
    end.

checkPossibility([]) -> true;
checkPossibility([H|T])  ->
    Size = length(H),
    if ( Size == 0 ) ->
        false;
    (1==1) ->
        checkPossibility(T)
    end.


createPartition([],_,_,Result,_) -> Result;
createPartition([H|T],Size,TempResult,Result,Counter) ->
    if (Counter == Size) ->
        NewRes = TempResult++[H],
        NewFullRes = Result++[NewRes],
        createPartition(T,Size,[],NewFullRes,1);
    (1==1) ->
        NewRes = TempResult++[H],
        createPartition(T,Size,NewRes,Result,Counter+1)
    end.


addToSolution(_,[],Result,_) -> Result;
addToSolution(SudokuSize,[PossH|PossT],Result,Counter)->
    PossibNumber = length(PossH),
    if (PossibNumber == 1) ->
        [TheValue|_] = PossH,
        NewRes = Result ++ [TheValue],
        addToSolution(SudokuSize,PossT,NewRes,Counter+1);
    (1==1) ->
        NewRes = Result ++ [1],
        addToSolution(SudokuSize,PossT,NewRes,Counter+1)
    end.

        
calculatePossibilites(_,_,[],_,_,Result,_,_,_,_) -> Result;
calculatePossibilites(SudokuSize,FullMx,[_|T],Row,Col,Result,Allrows,Allcols,Allsubcells,SimpleList) ->
    NewResult = Result ++ [ertekek({SudokuSize,FullMx},{Row,Col},Allrows,Allcols,Allsubcells,SimpleList)],
    if (Col == length(FullMx)) ->
        calculatePossibilites(SudokuSize,FullMx,T,Row+1,1,NewResult,Allrows,Allcols,Allsubcells,SimpleList);
    (1==1) ->
        calculatePossibilites(SudokuSize,FullMx,T,Row,Col+1,NewResult,Allrows,Allcols,Allsubcells,SimpleList)
    end.

addPossibleSteps([],[],Result) -> Result;
addPossibleSteps([],_,Result) -> Result;
addPossibleSteps(_,[],Result) -> Result;
addPossibleSteps([SolH|SolT],[PossH|PossT],Result) ->
    PossibNumber = length(PossH),
    if (PossibNumber == 1) ->
            [TheValue|_] = PossH,
            AlreadyContained = listFind(TheValue,SolH),
            if (AlreadyContained) ->
                NewRes = Result ++ [SolH],
                addPossibleSteps(SolT,PossT,NewRes);
            (1==1) ->
                Appended = append(SolH,[TheValue]),
                NewRes = Result ++ [Appended],
                addPossibleSteps(SolT,PossT,NewRes)
            end;
    (1==1) ->
        NewRes = Result ++ [SolH],
        addPossibleSteps(SolT,PossT,NewRes)
    end.
                


listFind(Element, List) ->
  lists:member(Element, List).

isqrt(0) -> 0;
isqrt(1) -> 1;
isqrt(X) when X >= 0 ->
    R = X div 2,
    isqrt(X div R, R, X).
isqrt(Q,R,X) when Q < R ->
    R1 = (R+Q) div 2,
    isqrt(X div R1, R1, X);
isqrt(_, R, _) -> R.






















