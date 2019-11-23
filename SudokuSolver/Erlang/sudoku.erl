-module(sudoku).
-author('vicsuka@gmail.com').
-vsn('2019-11-17').
-export([sudoku/1,calculatePossibilites/6,addFirstPossibleValue/7,addPossibleSteps/3,removeDoubleBrackets/2,multiplyList/4]).

-import(khf1,[feldarabolasa/2]).
-import(khf2,[ertekek/2]).
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
    ReSolution.

solveSudokuV2(Fullsize,Matrix,[],[],Counter,[]) ->
    SimpleMatrixList = feldarabolasa(Matrix,{1,1}),
    PossibleSteps = calculatePossibilites(isqrt(Fullsize),Matrix,SimpleMatrixList,1,1,[]),
    % SimpleList = addPossibleSteps(SimpleMatrixList, [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],[]),
    % %io:format("Possible: ~p ~n",[PossibleSteps]),

    solveSudokuV2(Fullsize,Matrix,PossibleSteps,SimpleMatrixList,Counter,[],[],0,[],[],[],0).


solveSudokuV2(Fullsize,Matrix,PossibleSteps,_,Counter,SolutionsArray,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter) ->    
    if (Counter > 20) ->
        io:format("COULD NOT SOLVE ~n");
    (1==1) ->           
        SimpleMatrixList = removeDoubleBrackets(feldarabolasa(Matrix,{1,1}),[]),
        %io:format("STEP: ~p ~n",[Counter]),
        %io:format("FOR MATRIX: ~p ~n",[Matrix]),
        %io:format("Possible: ~p ~n",[PossibleSteps]),
        %io:format("List: ~p ~n",[SimpleMatrixList]),

        %ADD VALUES THAT HAVE ONLY 1 POSSIBILITY
        NextSimpleList = addPossibleSteps(SimpleMatrixList,PossibleSteps,[]),
        %io:format("SURE VALUES: ~p ~n",[NextSimpleList]),

        if (NextSimpleList == SimpleMatrixList) ->

            %FIND BEST FIELD (SMALLEST POSSIBLE STEPS BIGGER THAN 1)
            Smallest = findSmallest(PossibleSteps,16),
            %io:format("Smallest: ~p ~n",[Smallest]),


            %ADD FIRST POSSIBLE VALUE TO MX
            {AdvancedSimpleList,NewBackUpArray,NewBackUpIndex} = addFirstPossibleValue(SimpleMatrixList, PossibleSteps, [], [], Smallest, 0, 1),
            %io:format("WE CHOSE: ~p, BACKUPS: ~p at: ~p ~n",[AdvancedSimpleList,NewBackUpArray,NewBackUpIndex]),

            %PARTITION CURRENT CONSTRAINT TO MATRIX
            NextMatrix = createPartition(AdvancedSimpleList,Fullsize,[],[],1),
            %io:format("NextMatrix: ~p ~n",[NextMatrix]),

            %NEXT POSSIBLE MOVES
            NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,AdvancedSimpleList,1,1,[]),
            %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

            %CURRENT SOLUTION FOR CHECKING
            ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
            PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
            %io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

            %LOGGING
            %io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

            %CHECK IF SUDOKU IS SOLVEABLE
            Possible = checkPossibility(NextPossibleSteps),

            IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
            %io:format("DONE? ~p ~n",[IsDone]),

            if (IsDone) ->
                NewsolutionAdded = SolutionsArray ++ PartitionedSolution,
                NewsolutionAdded;

                % Return = [NewsolutionAdded,{BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter}],
                % Return;
            (1==1) ->
                if (Possible) ->
                    Response = solveSudokuV2(Fullsize,NextMatrix,NextPossibleSteps,AdvancedSimpleList,Counter+1,SolutionsArray,NewBackUpArray,NewBackUpIndex,Matrix,PossibleSteps,SimpleMatrixList,Counter),
                    
                    if (is_list(Response)) ->
                        Response;
                        % %io:format("LIST: ~p ~n",[Response]),
                        % [AddSolutionX|BackUps] = Response,

                        % {Try2Array,Try2Index,Try2Matrix,Try2PossibleSteps,Try2SimpleList,Try2Counter} = hd(BackUps),

                        % NewPossibleSteps2 = lists:sublist(Try2PossibleSteps,Try2Index-1) ++ [Try2Array] ++ lists:nthtail(Try2Index,Try2PossibleSteps),
                        % solveSudokuV2(Fullsize,Try2Matrix,NewPossibleSteps2,Try2SimpleList,Try2Counter,AddSolutionX,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter);
                    (1==1) ->
                        {TryArray,TryIndex,TryMatrix,TryPossibleSteps,TrySimpleList,TryCounter} = Response,

                        NewPossibleSteps = lists:sublist(TryPossibleSteps,TryIndex-1) ++ [TryArray] ++ lists:nthtail(TryIndex,TryPossibleSteps),
                        solveSudokuV2(Fullsize,TryMatrix,NewPossibleSteps,TrySimpleList,TryCounter,SolutionsArray,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter)
                    end;
                (1==1) ->                
                    {BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter}
                end
            end;
        (1==1) ->
            %io:format("ADDED VALUE%!!!% ~n"),
            %PARTITION CURRENT CONSTRAINT TO MATRIX
            NextMatrix = createPartition(NextSimpleList,Fullsize,[],[],1),
            %io:format("NextMatrix: ~p ~n",[NextMatrix]),

            %NEXT POSSIBLE MOVES
            NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,NextSimpleList,1,1,[]),
            %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

            %CURRENT SOLUTION FOR CHECKING
            ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
            PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
            %io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

            %LOGGING
            %io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

            %CHECK IF SUDOKU IS SOLVEABLE
            Possible = checkPossibility(NextPossibleSteps),
            %io:format("Possible? ~p ~n",[Possible]),

            IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
            %io:format("DONE? ~p ~n",[IsDone]),

            if (IsDone) ->
                NewsolutionAdded = SolutionsArray ++ PartitionedSolution,
                NewsolutionAdded;

                % Return = [NewsolutionAdded,{BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter}],
                % Return;
            (1==1) ->
                if (Possible) ->
                    Response = solveSudokuV2(Fullsize,NextMatrix,NextPossibleSteps,NextSimpleList,Counter+1,SolutionsArray,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter),
                    if (is_list(Response)) ->
                        Response;
                        % %io:format("LIST: ~p ~n",[Response]),
                        % [AddSolutionX|BackUps] = Response, 
                        % {Try2Array,Try2Index,Try2Matrix,Try2PossibleSteps,Try2SimpleList,Try2Counter} = hd(BackUps),

                        % NewPossibleSteps2 = lists:sublist(Try2PossibleSteps,Try2Index-1) ++ [Try2Array] ++ lists:nthtail(Try2Index,Try2PossibleSteps),
                        % solveSudokuV2(Fullsize,Try2Matrix,NewPossibleSteps2,Try2SimpleList,Try2Counter,AddSolutionX,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter);2Matrix,NewPossibleSteps2,Try2SimpleList,Try2Counter,AddSolutionX,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter);
                    (1==1) ->
                        {TryArray,TryIndex,TryMatrix,TryPossibleSteps,TrySimpleList,TryCounter} = Response,

                        NewPossibleSteps = lists:sublist(TryPossibleSteps,TryIndex-1) ++ [TryArray] ++ lists:nthtail(TryIndex,TryPossibleSteps),
                        solveSudokuV2(Fullsize,TryMatrix,NewPossibleSteps,TrySimpleList,TryCounter,SolutionsArray,BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter)
                    end;
                (1==1) ->                
                    {BackUpArray,BackUpIndex,BackUpMatrix,BackUpPossibleSteps,BackUpSimpleList,BackUpCounter}
                end
            end
        
        end
        
    end.

solveSudokuV3(Fullsize,Matrix,[],[],Counter,[]) ->
    SimpleMatrixList = feldarabolasa(Matrix,{1,1}),
    PossibleSteps = calculatePossibilites(isqrt(Fullsize),Matrix,SimpleMatrixList,1,1,[]),
    % SimpleList = addPossibleSteps(SimpleMatrixList, [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],[]),
    % %io:format("Possible: ~p ~n",[PossibleSteps]),

    solveSudokuV3(Fullsize,Matrix,PossibleSteps,SimpleMatrixList,Counter,[]);


solveSudokuV3(Fullsize,Matrix,PossibleSteps,_,Counter,SolutionsArray) ->    
    if (Counter > 20) ->
        io:format("COULD NOT SOLVE ~n");
    (1==1) ->           
        SimpleMatrixList = removeDoubleBrackets(feldarabolasa(Matrix,{1,1}),[]),
        io:format("STEP: ~p ~n",[Counter]),
        io:format("FOR MATRIX: ~p ~n",[Matrix]),
        io:format("Possible: ~p ~n",[PossibleSteps]),
        io:format("List: ~p ~n",[SimpleMatrixList]),

        %ADD VALUES THAT HAVE ONLY 1 POSSIBILITY
        NextSimpleList = addPossibleSteps(SimpleMatrixList,PossibleSteps,[]),
        io:format("SURE VALUES: ~p ~n",[NextSimpleList]),

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
                io:format("AdvancedSimpleLists: ~p ~n",[AdvancedSimpleLists]),


                Results = doBranching(Fullsize,AdvancedSimpleLists,[],Counter),
                Results;
            
        (1==1) ->
            io:format("ADDED VALUE%!!!% ~n"),
            %PARTITION CURRENT CONSTRAINT TO MATRIX
            NextMatrix = createPartition(NextSimpleList,Fullsize,[],[],1),
            %io:format("NextMatrix: ~p ~n",[NextMatrix]),

            %NEXT POSSIBLE MOVES
            NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,NextSimpleList,1,1,[]),
            %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

            %CURRENT SOLUTION FOR CHECKING
            ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
            PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
            %io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

            %LOGGING
            io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

            %CHECK IF SUDOKU IS SOLVEABLE
            Possible = checkPossibility(NextPossibleSteps),
            %io:format("Possible? ~p ~n",[Possible]),

            IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
            io:format("DONE? ~p ~n",[IsDone]),

            if (IsDone) ->
                PartitionedSolution;
            (1==1) ->
                if (Possible) ->
                    solveSudokuV3(Fullsize,NextMatrix,NextPossibleSteps,NextSimpleList,Counter+1,SolutionsArray);
                (1==1) ->                
                    {Fullsize,Matrix}
                end
            end
        
        end
        
    end.

doBranching(_,[],Results,_) -> Results;
doBranching(Fullsize,[CurrentList|NextList], Results,Counter) ->
    %PARTITION CURRENT CONSTRAINT TO MATRIX
    NextMatrix = createPartition(CurrentList,Fullsize,[],[],1),
    %io:format("NextMatrix: ~p ~n",[NextMatrix]),

    %NEXT POSSIBLE MOVES
    NextPossibleSteps = calculatePossibilites(isqrt(Fullsize),NextMatrix,CurrentList,1,1,[]),
    %io:format("NextPossibleSteps: ~p ~n",[NextPossibleSteps]),

    %CURRENT SOLUTION FOR CHECKING
    ThisSolution = addToSolution(isqrt(Fullsize),NextPossibleSteps,[],0),
    PartitionedSolution = createPartition(ThisSolution,Fullsize,[],[],1),
    %io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

    %LOGGING
    %io:format("Current SOLUTION: ~p ~n",[PartitionedSolution]),

    %CHECK IF SUDOKU IS SOLVEABLE
    Possible = checkPossibility(NextPossibleSteps),

    IsDone = megoldase({isqrt(Fullsize),NextMatrix},PartitionedSolution),
    %io:format("DONE? ~p ~n",[IsDone]),

    if (IsDone) ->
        PartitionedSolution;
    (1==1) ->
        if (Possible) ->
            Solve = solveSudokuV3(Fullsize,NextMatrix,NextPossibleSteps,Results,Counter+1,Results),
            if (is_list(Solve)) ->
                NewResults = Results ++ [Solve],
                doBranching(Fullsize, NextList, NewResults,Counter);  
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

        
calculatePossibilites(_,_,[],_,_,Result) -> Result;
calculatePossibilites(SudokuSize,FullMx,[_|T],Row,Col,Result) ->
    NewResult = Result ++ [ertekek({SudokuSize,FullMx},{Row,Col})],
    if (Col == length(FullMx)) ->
        calculatePossibilites(SudokuSize,FullMx,T,Row+1,1,NewResult);
    (1==1) ->
        calculatePossibilites(SudokuSize,FullMx,T,Row,Col+1,NewResult)
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






















