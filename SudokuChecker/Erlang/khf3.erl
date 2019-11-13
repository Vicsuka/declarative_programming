-module(khf3).
-author('vicsuka@gmail.com').
-vsn('2019-11-13').
-export([megoldase/2]).
-import(string,[len/1,concat/2,chr/2,substr/3,str/2,to_lower/1,to_upper/1]).
-import(lists,[sublist/2,reverse/1,filter/2,append/1,flatlength/1]).
%-compile(export_all).

-spec khf3:megoldase(SSpec :: sspec(), SSol :: ssol()) -> B :: bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.

megoldase(Proposal, Solution)-> checkSol(Proposal,Solution).

checkSol(Proposal , Solution)->
    
    io:format("The value is: ~p,~p   .~n", [Proposal,Solution]),