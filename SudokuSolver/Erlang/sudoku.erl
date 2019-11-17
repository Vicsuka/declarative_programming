-module(sudoku).
-author('vicsuka@gmail.com').
-vsn('2019-11-17').
-export([sudoku/1,calculatePossibilites/6]).

-import(khf1,[feldarabolasa/2]).
-import(khf2,[ertekek/2]).
-import(khf3,[megoldase/2]).

-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1]).
-import(math,[sqrt/1]).

% -type sspec() :: {size(), board()}.
% -type size()  :: integer().
% -type field() :: [info()].
% -type info()  :: e | o | s | w | integer().
% -type board() :: [[field()]].

% -type ssol() :: [[integer()]].

% -spec sudoku:sudoku(SSpec :: sspec()) -> SSols :: [ssol()].
%% SSols az SSpec feladványt kielégítő megoldások listája.
sudoku(Matrix) -> solveProblems(Matrix, []).

solveProblems(Matrix, []) ->
    Fullsize = length(Matrix),
    Solution = solveSudoku(Fullsize,Matrix,[]),
    io:format("Solution: ~p ~n",[Solution]).

solveSudoku(Fullsize,Matrix,[]) ->
    SimpleList = feldarabolasa(Matrix,{1,1}),
    PossibleSteps = calculatePossibilites(isqrt(Fullsize),Matrix,SimpleList,1,1,[]),
    % io:format("Possible: ~p ~n",[PossibleSteps]),

    Added = addPossibleSteps(SimpleList,PossibleSteps,[]),
    % io:format("Added: ~p ~n",[Added]),

    NewSolution = addToSolution(isqrt(Fullsize),PossibleSteps,[],0),
    % io:format("NewSolution: ~p ~n",[NewSolution]),

    PartitionedAdded = createPartition(Added,Fullsize,[],[],1),
    % io:format("PartitionedAdded: ~p ~n",[PartitionedAdded]),

    PartitionedSolution = createPartition(NewSolution,Fullsize,[],[],1),
    % io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

    solveSudoku(Fullsize,PartitionedAdded,PartitionedSolution);


solveSudoku(Fullsize,Matrix,Solution) ->
    SimpleList = feldarabolasa(Matrix,{1,1}),
    IsDone = megoldase({isqrt(Fullsize),Matrix},Solution),
    if (IsDone) ->
        Solution;
    (1==1) ->
        PossibleSteps = calculatePossibilites(isqrt(Fullsize),Matrix,SimpleList,1,1,[]),
        io:format("Possible: ~p ~n",[PossibleSteps]),

        Added = addPossibleSteps(SimpleList,PossibleSteps,[]),
        % io:format("Added: ~p ~n",[Added]),

        NewSolution = addToSolution(isqrt(Fullsize),PossibleSteps,[],0),
        % io:format("NewSolution: ~p ~n",[NewSolution]),

        PartitionedAdded = createPartition(Added,Fullsize,[],[],1),
        % io:format("PartitionedAdded: ~p ~n",[PartitionedAdded]),

        PartitionedSolution = createPartition(NewSolution,Fullsize,[],[],1),
        % io:format("PartitionedSolution: ~p ~n",[PartitionedSolution]),

        solveSudoku(Fullsize,PartitionedAdded,PartitionedSolution)
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
addPossibleSteps([[SolH|_]|SolT],[PossH|PossT],Result) ->
    PossibNumber = length(PossH),
    if (PossibNumber == 1) ->
            [TheValue|_] = PossH,
            AlreadyContained = listFind(TheValue,SolH),
            if (AlreadyContained) ->
                NewRes = Result ++ [SolH],
                addPossibleSteps(SolT,PossT,NewRes);
            (1==1) ->
                NewRes = Result ++ [SolH ++ [TheValue]],
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


