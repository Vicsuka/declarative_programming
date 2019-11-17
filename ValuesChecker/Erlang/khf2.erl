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


ertekek(Proposal, Solution)-> checkField(Proposal,Solution).

checkField(Proposal,Solution) ->
        io:format("LENGTH: ~n").