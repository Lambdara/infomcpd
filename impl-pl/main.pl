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
is_addrsimple(lnnum(N)) :- number(N).
is_addrsimple(eof).
is_addrsimple(Regex) :- is_regex(Regex).

is_text(Text) :- string(Text).
is_label(Label) :- string(Label).
is_regex(Regex) :- string(Regex).
is_repl(Repl) :- string(Repl).
is_sflags(SFlags) :- string(SFlags).
is_str(Str) :- string(Str).

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
can_take_addr2(b(_)).
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


ss_abstract([], []).
ss_abstract([C | Cs], [A | As]) :- ss_abstract_c(C, A), ss_abstract(Cs, As).

ss_abstract_c(b(_), b(eof)).
ss_abstract_c(b(_, Label), b(Label)).
ss_abstract_c(t(_), b(eof)).
ss_abstract_c(t(_, Label), b(Label)).
ss_abstract_c(label(Label), label(Label)).
ss_abstract_c(block(_, Cmds), block(Cmds)).
ss_abstract_c(y(_, P, R), y(P, R)).
ss_abstract_c(Cmd, c) :- \+ member(Cmd, [b(_), b(_, _), t(_), t(_, _), label(_), block(_, _), y(_, _, _)]).

static_sem(Sed) :- ss_abstract(Sed, Abs), ss_ok(Abs).
ss_ok(S) :- ss_lc(S), ss_yc(S).
ss_lc(S) :- ss_labels(S, L), ss_lcp(L, S).
ss_labels([], []).
ss_labels([label(Lab) | S], [Lab | L]) :- ss_labels(S, L), \+ member(Lab, L).
ss_labels([block(S1) | S], L) :- append(S1, S, SS), ss_labels(SS, L).
ss_labels([b(_) | S], L) :- ss_labels(S, L).
ss_labels([y(_, _) | S], L) :- ss_labels(S, L).
ss_labels([c | S], L) :- ss_labels(S, L).
ss_lcp(_, []).
ss_lcp(L, [b(Lab) | S]) :- member(Lab, L), ss_lcp(L, S).
ss_lcp(L, [block(S1) | S]) :- ss_lcp(L, S1), ss_lcp(L, S).
ss_lcp(L, [label(_) | S]) :- ss_lcp(L, S).
ss_lcp(L, [y(_, _) | S]) :- ss_lcp(L, S).
ss_lcp(L, [c | S]) :- ss_lcp(L, S).
ss_yc([]).
ss_yc([y(P, R) | S]) :- string_length(P, Len), string_length(R, Len), ss_nodup(P), ss_yc(S).
ss_yc([block(S1) | S]) :- ss_yc(S1), ss_yc(S).
ss_yc([b(_) | S]) :- ss_yc(S).
ss_yc([label(_) | S]) :- ss_yc(S).
ss_yc([c | S]) :- ss_yc(S).
ss_nodup(Str) :- string_codes(Str, Codes), nodup(Codes).

nodup([]).
nodup([C | Cs]) :- \+ member(C, Cs), nodup(Cs).


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

incrIP([N | IP], [M | IP]) :- M is N + 1.
incrLnnum(S, S1) :- lnnum(S, K), K1 is K + 1, setlnnum(S, K1, S1).

toplevel(C, I, N, O) :- findlabels(C, L), newstate(C, I, L, N, S), nextcycle(S, S1), output(S1, O).

findlabels(C, L) :- findlabelsp(C, [0], L).
findlabelsp([], _, []).
findlabelsp([label(Lab) | C], IP, [pair(Lab, IP) | L]) :- incrIP(IP, IPp), findlabelsp(C, IPp, L).
findlabelsp([block(C2) | C], IP, LL) :- findlabelsp(C2, [0 | IP], L2), incrIP(IP, IPp), findlabelsp(C, IPp, L), append(L2, L, LL).
findlabelsp([Cmd | C], IP, L) :- \+ member(Cmd, [label(_), block(_)]), incrIP(IP, IPp), findlabelsp(C, IPp, L).

nextcycle(S, S) :- write("-- CYCLE --"), nl, writeq(S), nl, noinput(S).
nextcycle(S, S3) :- nextinput(S, S1, I), setpat(S1, I, S2), execcmd(S2, [0], S3).

endcycle(S, S2) :- endcycle_patwrite(S, S1), nextcycle(S1, S2).

endcycle_patwrite(S, S) :- nflag(S, true).
endcycle_patwrite(S, S1) :- nflag(S, false), pat(S, P), linetooutput(S, P, S1).

execcmd(S, IP, S1) :- writeq(S), nl, findcmd(S, IP, C), writeq(C), nl, perform(S, IP, C, S1).
execcmd(S, IP, S1) :- outrange(S, IP), exitblock(IP, IPp), execcmd(S, IPp, S1).
execcmd(S, [IP], S1) :- outrange(S, [IP]), endcycle(S, S1).

exitblock([_ | [N | IP]], [M | IP]) :- M is N + 1.

addrtag(not(addr2(Addr, Cmd)), not(Addr), Cmd).
addrtag(not(addr1(Addr, Cmd)), not(Addr), Cmd).
addrtag(addr2(Addr, Cmd), Addr, Cmd).
addrtag(addr1(Addr, Cmd), Addr, Cmd).

addrmatch(S, lnnum(N)) :- lnnum(S, N).
addrmatch(S, not(lnnum(N))) :- lnnum(S, M), N \= M.
addrmatch(S, eof) :- noinput(S).
addrmatch(S, not(eof)) :- haveinput(S).
addrmatch(S, a2(lnnum(N), lnnum(M))) :- lnnum(S, K), N =< K, K =< M.

addrmatch_not(S, not(A)) :- addrmatch(S, A).
addrmatch_not(S, lnnum(N)) :- addrmatch(S, not(lnnum(N))).
addrmatch_not(S, eof) :- addrmatch(S, not(eof)).
addrmatch_not(S, a2(A, B)) :- addrmatch(S, not(a2(A, B))).

perform(S, IP, A, S1) :- addrtag(A, Addr, Cmd), addrmatch(S, Addr), perform(S, IP, Cmd, S1).
perform(S, IP, A, S1) :- addrtag(A, Addr, _), addrmatch_not(S, Addr), incrIP(IP, IPp), execcmd(S, IPp, S1).

perform(S, IP, a(Text), S2) :- aq(S, AQ), string_concat(AQ, Text, AT), string_concat(AT, "\n", ATN), setaq(S, ATN, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, _, b(Lab), S1) :- lookuplabel(S, Lab, IPp), execcmd(S, IPp, S1).
perform(S, _, b, S1) :- endcycle(S, S1).
perform(S, _, d, S2) :- setpat(S, "", S1), nextcycle(S1, S2).
perform(S, _, dd, S2) :- pat(S, P), splitline(P, _, Rest), setpat(S, Rest, S1), execcmd(S1, [0], S2).
perform(S, IP, dd, S1) :- pat(S, P), nonewline(P), perform(S, IP, d, S1).
perform(S, IP, g, S2) :- hold(S, H), setpat(S, H, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, gg, S2) :- hold(S, H), pat(S, P), string_concat(P, "\n", PN), string_concat(PN, H, PNH), setpat(S, PNH, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, h, S2) :- pat(S, P), sethold(S, P, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, hh, S2) :- hold(S, H), pat(S, P), string_concat(H, "\n", HN), string_concat(HN, P, HNP), sethold(S, HNP, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, i(Text), S2) :- linetooutput(S, Text, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, _, n, S1) :- noinput(S), endcycle(S, S1).
perform(S, IP, n, S3) :- nextinput(S, S1, I), setpat(S1, I, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
perform(S, _, nn, S1) :- noinput(S), endcycle(S, S1).
perform(S, IP, nn, S3) :- nextinput(S, S1, I), pat(S1, P), string_concat(P, "\n", PN), string_concat(PN, I, PNI), setpat(S1, PNI, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
perform(S, IP, p, S2) :- pat(S, P), linetooutput(S, P, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, pp, S2) :- pat(S, P), splitline_lenient(P, Line, _), linetooutput(S, Line, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, _, q, S1) :- endcycle_patwrite(S, S1).
% TODO: s
perform(S, _, t(Lab), S2) :- tflag(S, true), lookuplabel(S, Lab, IPp), settflag(S, false, S1), execcmd(S1, IPp, S2).
perform(S, IP, t(_), S1) :- tflag(S, false), incrIP(IP, IPp), execcmd(S, IPp, S1).
perform(S, _, t, S2) :- tflag(S, true), settflag(S, false, S1), endcycle(S1, S2).
perform(S, IP, t, S1) :- tflag(S, false), incrIP(IP, IPp), execcmd(S, IPp, S1).
perform(S, IP, x, S3) :- pat(S, P), hold(S, H), setpat(S, H, S1), sethold(S1, P, S2), incrIP(IP, IPp), execcmd(S2, IPp, S3).
% TODO: y
perform(S, IP, label(_), S1) :- incrIP(IP, IPp), execcmd(S, IPp, S1).
perform(S, IP, lnnum, S2) :- lnnum(S, K), number_string(K, Str), linetooutput(S, Str, S1), incrIP(IP, IPp), execcmd(S1, IPp, S2).
perform(S, IP, block(_), S1) :- execcmd(S, [0 | IP], S1).

linetooutput(S, Line, S1) :- output(S, O), string_concat(O, Line, OL), string_concat(OL, "\n", OLN), setoutput(S, OLN, S1).

noinput(S) :- input(S, "").
haveinput(S) :- input(S, I), string_length(I, L), L > 0.
nextinput(S, S3, I) :- flushaq(S, S1), input(S1, Input), splitline(Input, I, Rest), setinput(S1, Rest, S2), incrLnnum(S2, S3).

flushaq(S, S2) :- output(S, O), aq(S, AQ), string_concat(O, AQ, OA), setoutput(S, OA, S1), setaq(S1, "", S2).

findcmd(S, IP, Cmd) :- code(S, C), reverse(IP, IPr), findcmd_c(C, IPr, Cmd).
findcmd_c([Cmd | _], [0], Cmd).
findcmd_c([BlockA | _], [0 | IP], Cmd) :- addrtag(BlockA, _, block(Cmds)), findcmd_c(Cmds, IP, Cmd).
findcmd_c([block(Cmds) | _], [0 | IP], Cmd) :- findcmd_c(Cmds, IP, Cmd).
findcmd_c([_ | C], [N | IP], Cmd) :- N > 0, M is N - 1, findcmd_c(C, [M | IP], Cmd).

outrange(S, IP) :- code(S, C), reverse(IP, IPr), outrange_c(C, IPr).
outrange_c([], _).
outrange_c([BlockA | _], [0 | IP]) :- addrtag(BlockA, _, block(Cmds)), outrange_c(Cmds, IP).
outrange_c([block(Cmds) | _], [0 | IP]) :- outrange_c(block(Cmds), IP).
outrange_c([_ | C], [N | IP]) :- N > 0, M is N - 1, outrange_c(C, [M | IP]).

splitline(Text, Line, Rest) :- string_length(Text, N), N > 0, splitline_lenient(Text, Line, Rest).
splitline_lenient(Text, Line, Rest) :- string_codes(Text, TextC), splitline_c(TextC, LineC, RestC), string_codes(Line, LineC), string_codes(Rest, RestC).
splitline_c([], [], []).
splitline_c([10 | Rest], [], Rest).
splitline_c([C | Text], [C | Line], Rest) :- C \= 10, splitline_c(Text, Line, Rest).

nonewline(Str) :- string_codes(Str, Codes), \+ member(10, Codes).
