:- dynamic 
	% predicate to store all chat content as individual facts converted to html,
	% numbered to keep track of items that appear multiple times (e.g., chat(5, 'hi')) 
	chat/2,
	% counter to keep track of number of chat items
	chatCounter/1,
	renderPage/0.

header('<nav class="navbar mb-5"><div class="navbar-nav vu_logo"></div></nav>').
main('<main class="container overflow-auto d-flex flex-column-reverse text-center" style="height: 67vh;">~a</main>').
footer('<footer class="fixed-bottom">~a</footer>').

html(Main,Footer,Html) :- header(H), main(MT), format(atom(M),MT,[Main]), 
	footer(FT), format(atom(F),FT,[Footer]), atom_concat(H,M,HM), atom_concat(HM,F,Html).

%text('<h1>~a</h1>').
%text(Content,Html) :- text(T), format(atom(Html),T,[Content]).

paragraph('<p>~a</p>').
paragraph(Content,Html) :- paragraph(T), format(atom(Html),T,[Content]).

%img('<img src="~a"/>').
%img(Src,Html) :- img(I), format(atom(Html),I,[Src]).

%button('<button class="btn btn-primary btn-lg mt-5 ml-3">~a</button>').
%button(Content,Html) :- button(B), format(atom(Html),B,[Content]).

%buttons([],Html,Html).
%buttons([Curr|Rest],Tmp,Html) :- button(Curr,B), atom_concat(Tmp,B,New), buttons(Rest,New,Html).
%buttons(ContentList,Html) :- buttons(ContentList,'',Html).

chatbox('<div class="chatbox mb-3 text-center"></div>').