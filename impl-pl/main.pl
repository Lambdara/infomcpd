%% data true cmd, data false cmd

is_sed([]).
is_sed([C | S]) :- is_cmd(C), is_sed(S).

is_addr2(not(A)) :- is_addr2n(A).
is_addr2(A) :- is_addr2n(A).
is_addr1(not(A)) :- is_addr1n(A).
is_addr1(A) :- is_addr1n(A).
is_addr2n(a2(A, B)) :- is_addrsimple(A), is_addrsimple(B).
is_addr2n(A) :- is_addr1n(A).
is_addr1n(A) :- is_addrsimple(A).
is_addr1n(none).
is_addrsimple(ln(N)) :- number(N).
is_addrsimple(eof).
is_addrsimple(Regex) :- is_regex(Regex).

is_text(Text) :- string(Text).
is_label(Label) :- string(Label).
is_regex(Regex) :- string(Regex).
is_repl(Repl) :- string(Repl).
is_sflags(SFlags) :- string(SFlags).
is_str(Str) :- string(Str).

%% data block cmd, data a cmd, data b cmd, data c cmd, data d cmd, data dd cmd
%% data g cmd, data gg cmd, data h cmd, data hh cmd, data i cmd, data n cmd
%% data nn cmd, data p cmd, data pp cmd, data q cmd, data s cmd, data t cmd
%% data x cmd, data y cmd, data label cmd, data lnnum cmd
%% predname label :
is_cmd(block(Cmds)) :- is_sed(Cmds).
is_cmd(a(Text)) :- is_text(Text).
is_cmd(b).
is_cmd(b(Label)) :- is_label(Label).
is_cmd(c(Text)) :- is_text(Text).
is_cmd(d).
is_cmd(dd).
is_cmd(g).
is_cmd(gg).
is_cmd(h).
is_cmd(hh).
is_cmd(i(Text)) :- is_text(Text).
is_cmd(n).
is_cmd(nn).
is_cmd(p).
is_cmd(pp).
is_cmd(q).
is_cmd(s(Regex, Repl, SFlags)) :- is_regex(Regex), is_repl(Repl), is_sflags(SFlags).
is_cmd(t).
is_cmd(t(Label)) :- is_label(Label).
is_cmd(x).
is_cmd(y(Str1, Str2)) :- is_str(Str1), is_str(Str2).
is_cmd(label(Label)) :- is_label(Label).
is_cmd(lnnum).
is_cmd(addr2(Addr, Cmd)) :- is_addr2(Addr), can_take_addr2(Cmd), is_cmd(Cmd).
is_cmd(addr1(Addr, Cmd)) :- is_addr1(Addr), can_take_addr1(Cmd), is_cmd(Cmd).

can_take_addr2(block(_)).
can_take_addr2(b).
can_take_addr2(b(_)).
can_take_addr2(c(_)).
can_take_addr2(d).
can_take_addr2(dd).
can_take_addr2(g).
can_take_addr2(gg).
can_take_addr2(h).
can_take_addr2(hh).
can_take_addr2(n).
can_take_addr2(nn).
can_take_addr2(p).
can_take_addr2(pp).
can_take_addr2(s(_, _, _)).
can_take_addr2(t).
can_take_addr2(t(_)).
can_take_addr2(x).
can_take_addr2(y(_, _)).

can_take_addr1(a(_)).
can_take_addr1(i(_)).
can_take_addr1(q).
can_take_addr1(lnnum).
can_take_addr1(Cmd) :- can_take_addr2(Cmd).


%% left ss_abstract 1, predname ss_abstract abstract
ss_abstract([], []).
%% left ss_abstract_c 1
%% predname ss_abstract_c abstract'
ss_abstract([C | Cs], [A | As]) :- ss_abstract_c(C, A), ss_abstract(Cs, As).

%% var Label=\ell
ss_abstract_c(b(Label), b(Label)).
ss_abstract_c(t(Label), b(Label)).
ss_abstract_c(label(Label), label(Label)).
ss_abstract_c(block(S), block(S)).
ss_abstract_c(y(Pat, Repl), y(Pat, Repl)).
ss_abstract_c(addr1(_Addr, C), A) :- ss_abstract_c(C, A).
ss_abstract_c(addr2(_Addr, C), A) :- ss_abstract_c(C, A).
ss_abstract_c(Cmd, c) :- \+ member(Cmd, [b(_), t(_), label(_), block(_), y(_, _), addr1(_, _), addr2(_, _)]).

%% left static_sem 1, left ss_ok 1
%% predname ss_ok ok, predname ss_lc lc, predname ss_yc yc
static_sem(Sed) :- ss_abstract(Sed, Abs), ss_ok(Abs).
%% left ss_lc 1, left ss_yc 1
ss_ok(S) :- ss_lc(S), ss_yc(S).
%% left ss_labels 1, left ss_lcp 2, predname ss_lcp lc', var L=L
%% predname ss_labels labels
ss_lc(S) :- ss_labels(S, L), ss_lcp(L, S).
%% var L=L, var Lab=\ell
ss_labels([], []).
ss_labels([label(Lab) | S], [Lab | L]) :- ss_labels(S, L), \+ member(Lab, L).
%% mathvar S1, var S1=s_1
ss_labels([block(S1) | S], L) :- append(S1, S, SS), ss_labels(SS, L).
ss_labels([b(_Lab) | S], L) :- ss_labels(S, L).
ss_labels([y(_Pat, _Repl) | S], L) :- ss_labels(S, L).
ss_labels([c | S], L) :- ss_labels(S, L).
%% var L=L, var Lab=\ell, var S1=s_1
ss_lcp(_L, []).
ss_lcp(L, [b(Lab) | S]) :- member(Lab, L), ss_lcp(L, S).
ss_lcp(L, [block(S1) | S]) :- ss_lcp(L, S1), ss_lcp(L, S).
ss_lcp(L, [label(_Lab) | S]) :- ss_lcp(L, S).
ss_lcp(L, [y(_Pat, _Repl) | S]) :- ss_lcp(L, S).
ss_lcp(L, [c | S]) :- ss_lcp(L, S).
ss_yc([]).
%% left ss_nodup 1, var S1=s_1, var Lab=\ell
%% predname ss_nodup nodup
%% left string_length 1
ss_yc([y(Pat, Repl) | S]) :- string_length(Pat, Len), string_length(Repl, Len), ss_nodup(Pat), ss_yc(S).
ss_yc([block(S1) | S]) :- ss_yc(S1), ss_yc(S).
ss_yc([b(_Lab) | S]) :- ss_yc(S).
ss_yc([label(_Lab) | S]) :- ss_yc(S).
ss_yc([c | S]) :- ss_yc(S).
%% left nodupp 1
%% predname nodupp nodup'
%% left string_codes 1
ss_nodup(Str) :- string_codes(Str, Codes), nodupp(Codes).

nodupp([]).
nodupp([C | Cs]) :- \+ member(C, Cs), nodup(Cs).


%% left newstate 4
%% data state tuple
newstate(C, I, L, N, state(C, I, "", "", "", "", L, false, N, 0)).
%% left code 1, left input 1, left output 1, left pat 1, left hold 1, left aq 1, left labels 1, left tflag 1, left nflag 1, left lnnum 1
code(  state(C, _, _, _, _, _, _, _, _, _), C).
input( state(_, I, _, _, _, _, _, _, _, _), I).
output(state(_, _, O, _, _, _, _, _, _, _), O).
pat(   state(_, _, _, P, _, _, _, _, _, _), P).
hold(  state(_, _, _, _, H, _, _, _, _, _), H).
aq(    state(_, _, _, _, _, Q, _, _, _, _), Q).
labels(state(_, _, _, _, _, _, L, _, _, _), L).
tflag( state(_, _, _, _, _, _, _, T, _, _), T).
nflag( state(_, _, _, _, _, _, _, _, N, _), N).
lnnum( state(_, _, _, _, _, _, _, _, _, K), K).
%% left setcode 2, left setinput 2, left setoutput 2, left setpat 2, left sethold 2, left setaq 2, left setlabels 2, left settflag 2, left setnflag 2, left setlnnum 2
setcode(  state(_, I, O, P, H, Q, L, T, N, K), C, state(C, I, O, P, H, Q, L, T, N, K)).
setinput( state(C, _, O, P, H, Q, L, T, N, K), I, state(C, I, O, P, H, Q, L, T, N, K)).
setoutput(state(C, I, _, P, H, Q, L, T, N, K), O, state(C, I, O, P, H, Q, L, T, N, K)).
setpat(   state(C, I, O, _, H, Q, L, T, N, K), P, state(C, I, O, P, H, Q, L, T, N, K)).
sethold(  state(C, I, O, P, _, Q, L, T, N, K), H, state(C, I, O, P, H, Q, L, T, N, K)).
setaq(    state(C, I, O, P, H, _, L, T, N, K), Q, state(C, I, O, P, H, Q, L, T, N, K)).
setlabels(state(C, I, O, P, H, Q, _, T, N, K), L, state(C, I, O, P, H, Q, L, T, N, K)).
settflag( state(C, I, O, P, H, Q, L, _, N, K), T, state(C, I, O, P, H, Q, L, T, N, K)).
setnflag( state(C, I, O, P, H, Q, L, T, _, K), N, state(C, I, O, P, H, Q, L, T, N, K)).
setlnnum( state(C, I, O, P, H, Q, L, T, N, _), K, state(C, I, O, P, H, Q, L, T, N, K)).

%% left incrIP 1, arg incrIP 1 stack, arg incrIP 2 stack
incrIP([N | IP], [M | IP]) :- M is N + 1.
%% left incrlnnum 1, var S1=s_1
incrlnnum(S, S1) :- lnnum(S, K), K1 is K + 1, setlnnum(S, K1, S1).

%% left toplevel 3, left findlabels 1, left nextcycle 1
%% var S1=s_1
toplevel(C, I, N, O) :- findlabels(C, L), newstate(C, I, L, N, S), nextcycle(S, S1), output(S1, O).

%% left findlabelsp 2, predname findlabelsp findlabels', arg findlabelsp 2 stack
findlabels(C, L) :- findlabelsp(C, [0], L).
%% var IPp=ip', var C2=c_2, var L2=l_2
findlabelsp([], _, []).
%% data pair tuple
findlabelsp([label(Lab) | C], IP, [pair(Lab, IP) | L]) :- incrIP(IP, IPp), findlabelsp(C, IPp, L).
findlabelsp([block(C2) | C], IP, LL) :- findlabelsp(C2, [0 | IP], L2), incrIP(IP, IPp), findlabelsp(C, IPp, L), append(L2, L, LL).
findlabelsp([Cmd | C], IP, L) :- \+ member(Cmd, [label(_), block(_)]), incrIP(IP, IPp), findlabelsp(C, IPp, L).

%% left noinput 1
nextcycle(S, S) :- write("-- CYCLE --"), nl, writeq(S), nl, noinput(S).
%% left execcmd 2, left nextinput 1, var S1=s_1, var S2=s_2, var S3=s_3
%% arg execcmd 2 stack
nextcycle(S, S3) :- nextinput(S, S1, I), setpat(S1, I, S2), execcmd(S2, [0], S3).

%% left endcycle 1, left endcycle_patwrite 1, var S1=s_1, var S2=s_2, var S3=s_3
endcycle(S, S2) :- endcycle_patwrite(S, S1), nextcycle(S1, S2).

endcycle_patwrite(S, S) :- nflag(S, true).
%% left linetooutput 2, var S1=s_1
endcycle_patwrite(S, S1) :- nflag(S, false), pat(S, P), linetooutput(S, P, S1).

%% left findcmd 2, left perform 3, var S1=s_1
%% arg findcmd 2 stack
execcmd(S, IP, S1) :- writeq(S), nl, findcmd(S, IP, C), writeq(C), nl, perform(S, IP, C, S1).
%% var IPp=ip'
%% left outrange 2, left exitblock 1
%% arg outrange 2 stack
execcmd(S, IP, S1) :- outrange(S, IP), exitblock(IP, IPp), execcmd(S, IPp, S1).
execcmd(S, [IP], S1) :- outrange(S, [IP]), endcycle(S, S1).

%% arg exitblock 1 stack, arg exitblock 2 stack
exitblock([_ | [N | IP]], [M | IP]) :- M is N + 1.

%% left addrtag 1
addrtag(addr2(Addr, Cmd), Addr, Cmd).
addrtag(addr1(Addr, Cmd), Addr, Cmd).

%% left addrmatch 2
addrmatch(S, ln(N)) :- lnnum(S, N).
addrmatch(S, not(ln(N))) :- lnnum(S, M), N \= M.
addrmatch(S, eof) :- noinput(S).
%% left haveinput 1
addrmatch(S, not(eof)) :- haveinput(S).
addrmatch(S, a2(ln(N), ln(M))) :- lnnum(S, K), N =< K, K =< M.
addrmatch(S, not(a2(ln(N), ln(_M)))) :- lnnum(S, K), K < N.
addrmatch(S, not(a2(ln(_N), ln(M)))) :- lnnum(S, K), M < K.

%% left addrmatch_not 2
addrmatch_not(S, not(A)) :- addrmatch(S, A).
addrmatch_not(S, ln(N)) :- addrmatch(S, not(ln(N))).
addrmatch_not(S, eof) :- addrmatch(S, not(eof)).
addrmatch_not(S, a2(A, B)) :- addrmatch(S, not(a2(A, B))).

%% var IPp=ip', var S1=s_1, var S2=s_2, var S3=s_3, var Lab=\ell
perform(S, IP, A, S1) :- addrtag(A, Addr, Cmd), addrmatch(S, Addr), perform(S, IP, Cmd, S1).
perform(S, IP, A, S1) :- addrtag(A, Addr, _Cmd), addrmatch_not(S, Addr), incrIP(IP, IPp), execcmd(S, IPp, S1).

perform(S, IP, a(Text), S2) :- aq(S, AQ), string_concat(AQ, Text, AT), string_concat(AT, "\n", ATN), setaq(S, ATN, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
%% left lookuplabel 2
perform(S, _IP, b(Lab), S1) :- lookuplabel(S, Lab, IPp), execcmd(S, IPp, S1).
perform(S, _IP, b, S1) :- endcycle(S, S1).
perform(S, _IP, d, S2) :- setpat(S, "", S1), nextcycle(S1, S2).
%% left splitline 1
perform(S, _IP, dd, S2) :- pat(S, P), splitline(P, _Line, Rest), setpat(S, Rest, S1), execcmd(S1, [0], S2).
%% left nonewline 1
perform(S, IP, dd, S1) :- pat(S, P), nonewline(P), perform(S, IP, d, S1).
perform(S, IP, g, S2) :- hold(S, H), setpat(S, H, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, gg, S2) :- hold(S, H), pat(S, P), string_concat(P, "\n", PN), string_concat(PN, H, PNH), setpat(S, PNH, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, h, S2) :- pat(S, P), sethold(S, P, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, hh, S2) :- hold(S, H), pat(S, P), string_concat(H, "\n", HN), string_concat(HN, P, HNP), sethold(S, HNP, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, i(Text), S2) :- linetooutput(S, Text, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, _IP, n, S1) :- noinput(S), endcycle(S, S1).
perform(S, IP, n, S3) :- nextinput(S, S1, I), setpat(S1, I, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
perform(S, _IP, nn, S1) :- noinput(S), endcycle(S, S1).
perform(S, IP, nn, S3) :- nextinput(S, S1, I), pat(S1, P), string_concat(P, "\n", PN), string_concat(PN, I, PNI), setpat(S1, PNI, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
perform(S, IP, p, S2) :- pat(S, P), linetooutput(S, P, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, pp, S2) :- pat(S, P), splitline_lenient(P, Line, _Rest), linetooutput(S, Line, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, _IP, q, S1) :- endcycle_patwrite(S, S1).
% TODO: s
perform(S, _IP, t(Lab), S2) :- tflag(S, true), lookuplabel(S, Lab, IPp), settflag(S, false, S1), execcmd(S1, IPp, S2).
perform(S, IP, t(_Lab), S1) :- tflag(S, false), incrIP(IP, IPp), execcmd(S, IPp, S1).
perform(S, _IP, t, S2) :- tflag(S, true), settflag(S, false, S1), endcycle(S1, S2).
perform(S, IP, t, S1) :- tflag(S, false), incrIP(IP, IPp), execcmd(S, IPp, S1).
perform(S, IP, x, S3) :- pat(S, P), hold(S, H), setpat(S, H, S1), sethold(S1, P, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
% TODO: y
perform(S, IP, label(_Lab), S1) :- incrIP(IP, IPp), execcmd(S, IPp, S1).
%% left number_string 1
perform(S, IP, lnnum, S2) :- lnnum(S, K), number_string(K, Str), linetooutput(S, Str, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, block(_Lab), S1) :- execcmd(S, [0 | IP], S1).

%% predname lookuplabelp lookuplabel', left lookuplabelp 2
%% var L=L, var Lab=\ell
lookuplabel(S, Lab, IP) :- labels(S, L), lookuplabelp(Lab, L, IP).
%% var L=L, var Lab=\ell, var Lab2=\ell_2, var IPp=ip'
lookuplabelp(Lab, [pair(Lab, IP) | _L], IP).
lookuplabelp(Lab, [pair(Lab2, _IPp) | L], IP) :- Lab \= Lab2, lookuplabelp(Lab, L, IP).

%% var S1=s_1
linetooutput(S, Line, S1) :- output(S, O), string_concat(O, Line, OL), string_concat(OL, "\n", OLN), setoutput(S, OLN, S1).

noinput(S) :- input(S, "").
haveinput(S) :- input(S, I), string_length(I, Len), Len > 0.
%% left flushaq 1, var S1=s_1, var S2=s_2, var S3=s_3
nextinput(S, S3, I) :- flushaq(S, S1), input(S1, Input), splitline(Input, I, Rest), setinput(S1, Rest, S2), incrlnnum(S2, S3).

%% var S1=s_1, var S2=s_2
flushaq(S, S2) :- output(S, O), aq(S, AQ), string_concat(O, AQ, OA), setoutput(S, OA, S1), setaq(S1, "", S2).

%% predname findcmd_c findcmd'
%% left findcmd_c 2, left reverse 1
%% arg findcmd_c 2 stack
findcmd(S, IP, Cmd) :- code(S, C), reverse(IP, IPr), findcmd_c(C, IPr, Cmd).
findcmd_c([Cmd | _Rest], [0], Cmd).
findcmd_c([BlockA | _Rest], [0 | IPr], Cmd) :- addrtag(BlockA, _Addr, block(Cmds)), findcmd_c(Cmds, IPr, Cmd).
findcmd_c([block(Cmds) | _Rest], [0 | IPr], Cmd) :- findcmd_c(Cmds, IPr, Cmd).
findcmd_c([_Cmd | Rest], [N | IPr], Cmd) :- N > 0, M is N - 1, findcmd_c(Rest, [M | IPr], Cmd).

%% predname outrange_c outrange'
%% left outrange_c 2
%% arg outrange_c 2 stack
outrange(S, IP) :- code(S, C), reverse(IP, IPr), outrange_c(C, IPr).
outrange_c([], _IPr).
outrange_c([BlockA | _Rest], [0 | IPr]) :- addrtag(BlockA, _Addr, block(Cmds)), outrange_c(Cmds, IPr).
outrange_c([block(Cmds) | _Rest], [0 | IPr]) :- outrange_c(Cmds, IPr).
outrange_c([_Cmd | C], [N | IPr]) :- N > 0, M is N - 1, outrange_c(C, [M | IPr]).

%% left splitline_lenient 1
splitline(Text, Line, Rest) :- string_length(Text, N), N > 0, splitline_lenient(Text, Line, Rest).
%% predname splitline_c splitline', left splitline_c 1
splitline_lenient(Text, Line, Rest) :- string_codes(Text, TextC), splitline_c(TextC, LineC, RestC), string_codes(Line, LineC), string_codes(Rest, RestC).
splitline_c([], [], []).
splitline_c([10 | Rest], [], Rest).
splitline_c([C | Text], [C | Line], Rest) :- C \= 10, splitline_c(Text, Line, Rest).

nonewline(Str) :- string_codes(Str, Codes), \+ member(10, Codes).
