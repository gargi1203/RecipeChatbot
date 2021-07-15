%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversation agenda	                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic
	% An agenda/1 is a list of pattern names that the agent should use in that order to complete
	% its interaction agenda with a user.
	agenda/1,
	% The initial agenda set at the start of a conversation is stored using the initialAgenda/1 
	% predicate, i.e. this predicate stores a copy of the agenda at the start of a session.
	initialAgenda/1,
	% The session/1 history is a list of sequences. Initially this is the empty list. There are 
	% a number of constraints on the session history:
	% - only the sequence at the head of the list can be incomplete (but need not be);
	% - the first element of a sequence is a pattern id;
	% - the other elements of a sequence are either
	%   (a) intent triples of the form [actor, intent, parameters] with actor either
	% 	'user' or 'agent', or
	%   (b) subsequences of the form [pattern id, ...] with pattern id a label (constant) and 
	%       ... a list of intent triples and subsequences;
	% - a (sub)sequence is (in)complete if its sublist of intent triples (does not) matches the
	%   intent triples of its associated pattern (references by the pattern id).
	session/1.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory: a memory is a list of entity key, value pairs  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A memory/1 is a store for recording user parameter input (entity key-value pairs).
:- dynamic memory/1.

% Retrieves a value for an entity parameter (or key) and make sure values are all lower case.
keyValue(EntityKey, EntityValue)
	:- memory(Params), member(EntityKey = EntityValue, Params). %, string_lower(Value, EntityValue).

% updateMemory/3 is a helper predicate for memory updates.
% updateMemory(+NewParams, -OldMemory, -NewMemory)
% @NewParams: list of entity key-value pairs of the form [key1 = value1, key2 = value2 , ...]
% @OldMemory: old memory = list of entity key-value pairs
% @NewMemory: updated memory with new parameters
updateMemory(NewParams, OldMemory, NewMemory) :- 
	memory(OldMemory), 
	removeEmptyParams(NewParams, Params),
	setof(Key, V^member(Key=V, Params), Keys), % fails if Params = [], which is fine because no update is needed in that case
	remove(Keys, OldMemory, CleanedParams), 
	append(Params, CleanedParams, NewMemory).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behavioral parameters                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic
	% the name of the dialogue agent.
	agentName/1,
	% name of user male
	%userMaleName/1,
	% name of user female
	%userFemaleName/1,
	% indicates whether the agent should perform a last topic check at the end of a session.
	lastTopicCheck/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Administrative predicates                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic
	% used to identify slots (entity parameters) that have not been filled yet.
	missingSlots/1,
	% stepCounter/1 is used to implement the repeat(SubPattern) agenda instruction. This counter 
	% counts steps performed when executing this instruction and is initialized to -1 to 
	% indicate that the stepCounter has not been used yet.
	stepCounter/1,
	% totalSteps/1 is used to set the target total number of steps to perform when the repeat
	% agenda instruction is executed.
	totalSteps/1.

/**
 * missingSlots(+Params, -Missing)
 *
 * Identifies any empty slots in parameters (of intent triple) that cannot be retrieved from memory 
 * either.
 *
 * @param Params: list of the form [param1 = .., param2 = .., ...].
 * @param Missing: list of missing parameters (slots) of the form [paramM, paramN, ...].
**/
missingSlots(Params, Missing) :- 
	findall(Key, member(Key = '', Params), EmptyKeys),
	memory(Memory), findall(Key, member(Key=_, Memory), Keys),
	subtract(EmptyKeys, Keys, Missing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sequence handling			                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A list that is part of a session is a sequence if it starts with a pattern id
% (i.e. constant that is not 'agent' or 'user')
sequence([H | _]) :- not(is_list(H)), not(H = agent), not(H = user).

/**
 * add(+Sequence, +Item, UpdatedSequence).
 *
 * Adds a given item to a given sequence. Adds the item to the the currently active and
 * incomplete (sub)sequence.
 *
 * @param Sequence A sequence, i.e. a list starting with a pattern id (head) followed by intent
 *			triples and subsequences (not the complete session history!).
 * @param Item Either an intent triple of the form [Actor, Intent, Params], 
 *		or an empty sequence, i.e. a list with a pattern id of the form [PatterId].
 * @param UpdatedSequence The given sequence updated by adding the item.  
**/
% Definition by induction on the structure of a sequence.
% Case of single pattern id.
add([PatternId], Item, NewSequence) :-
	not(is_list(PatternId)), append([PatternId], [Item], NewSequence).
% Case of single intent triple.
add([IntentTriple], Item, NewSequence) :-
	intentTriple(IntentTriple), append([IntentTriple], [Item], NewSequence).
% Case of completed subsequence as last element of sequence.
add([SubSequence], Item, NewSequence) :-
	completedPattern(SubSequence), append([SubSequence], [Item], NewSequence).
% Case of incomplete subsequence as last element of sequence.
% Add intent triple to this subsequence because by assumption it should match associated pattern.
add([SubSequence], Item, [NewSubSequence]) :-
	sequence(SubSequence), not(completedPattern(SubSequence)), 
	add(SubSequence, Item, NewSubSequence).
% Case where there are at least two elements in the sequence (where first element can also be a
% pattern id).
add([El1, El2 | Sequence], Item, [El1 | NewSequence]) :-
	add([El2 | Sequence], Item, NewSequence).

% Check if sequence is complete by matching it against associated pattern.
completedPattern([PatternId | Sequence]) :-
	getActorIntentPairs(Sequence, ActorIntentPairs),
	pattern([ PatternId | PatternSequence ]),
	append(ActorIntentPairs, [], PatternSequence).

% Find pattern where first intent matches given intent.
% Pattern b13 for handling out of context dialog moves where first user intent matches any intent
% must be excluded
matchingPattern(Actor, Intent, PatternId) :-
	pattern([ PatternId, [Actor, Intent] | _ ]), not(PatternId = b13).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compute expected intent		                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * expectedIntent(-Actor, -Intent).
 *
 * Infer the actor and intent that are expected next from the (variant of the) pattern associated
 * with the currently active (sub)sequence and the intent triples part of that sequence;
 * uses the fact that only the sequence at the head of a session can be incomplete (but need not be).
 *
 * @param Actor Either 'agent' or 'user', whoever is expected to contribute to the interaction next.
 * @param Intent The intent (label) that is expected next.  
**/
expectedIntent(Actor, Intent) :- session([ Sequence | _ ]), expectedIntent(Sequence, Actor, Intent).

/**
 * expectedIntent(+Sequence, -Actor, -Intent).
 *
 * Helper predicate to implement expectedIntent(-Actor, -Intent).
 *
 * @param Sequence The currently active sequence (at the head of the session history).
 * @param Actor Either 'agent' or 'user', whoever is expected to contribute to the interaction next.
 * @param Intent The intent (label) that is expected next.  
**/
% First, compute the expected actor-intent pair for the case where the last sequence element is a 
% subsequence (starting with a pattern id constant); if such a pair is found, stop searching (cut).
expectedIntent(Sequence, Actor, Intent) :-
	last(Sequence, [ PatternId | SubSequence ]), not(is_list(PatternId)), 
	expectedIntent([PatternId | SubSequence], Actor, Intent), !.
% Second, compute the expected actor-intent pair by matching the list of actor-intent pairs (ignoring
% any entitiy key-value pairs) in the given sequence with its associated pattern.
expectedIntent([ PatternId | Sequence ], Actor, Intent) :-
	getActorIntentPairs(Sequence, ActorIntentPairs),
	pattern([ PatternId | PatternSequence ]),
	append(ActorIntentPairs, [ [Actor, Intent] | _ ], PatternSequence).

% Extract (actor, intent) pairs from given sequence; 
% remove parameters from intent triples and remove subsequences.
getActorIntentPairs([], []).
getActorIntentPairs([ [Actor, Intent, _ ] | Sequence ], [ [Actor, Intent] | ActorIntentPairs ]) :-
	intentTriple([Actor, Intent, _]), getActorIntentPairs(Sequence, ActorIntentPairs).
getActorIntentPairs([ Head | Sequence ], ActorIntentPairs) :-
	not(intentTriple(Head)), getActorIntentPairs(Sequence, ActorIntentPairs).

% Check if a sequence element is an intent triple (list of three items starting with 'agent' or 'user')
intentTriple([user, _, _]).
intentTriple([agent, _, _]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper predicates			                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * lastAddedIntentTriple(-IntentTriple)
 *
 * Retrieves the intent that has been added last to the currently active (sub)sequence in the
 * session history.
 *
 * @param IntentTriple The intent triple that was added last to the session history.
**/
lastAddedIntentTriple(IntentTriple) :- 
	session([ Sequence | _ ]), lastAddedIntentTriple(Sequence, IntentTriple).
	
/**
 * lastAddedIntentTriple(+Sequence, -IntentTriple)
 *
 * Retrieves the intent that has been added last to the sequence.
 *
 * @param Sequence A list representing part of a sequence (a list starting with a pattern id).
 * @param IntentTriple The intent triple that was added last to the session history.
**/
% Definition by induction on the structure of a sequence.
% Case single pattern id: in this case, there has not been any intent added, so fail.
lastAddedIntentTriple([PatternId], _) :- not(is_list(PatternId)), fail.
% Case single intent triple:
lastAddedIntentTriple([IntentTriple], IntentTriple) :-
	intentTriple(IntentTriple).
% Case subsequence:
lastAddedIntentTriple([SubSequence], IntentTriple) :- is_list(SubSequence), 
	lastAddedIntentTriple(SubSequence, IntentTriple).
% Case where there are at least two elements in the sequence (where first element can also be
% a pattern id):
lastAddedIntentTriple([_, El2 | Sequence], IntentTriple) :-
	lastAddedIntentTriple([El2 | Sequence], IntentTriple).

/**
 * removeEmptyParams(+Params, -NonEmptyParams)
 *
 * Removes key-value pairs from Params where value is empty (= '').
 *
 * @param Params: list of entity key-value pairs of the form [key1 = value1, key2 = value2 , ...],
 * 			possibly with key = '' (empty parameters).
 * @param NonEmptyParams: list of only those entity key-value pairs with non-empty value (not '').
**/
removeEmptyParams([], []).
removeEmptyParams([Key = Value | OldParams], [Key = Value | CleanedParams]) :- 
	not(Value = ''), removeEmptyParams(OldParams, CleanedParams).
removeEmptyParams([_ = '' | OldParams], CleanedParams) :- 
	removeEmptyParams(OldParams, CleanedParams).
/**
 * remove(+Keys, +OldParams, -CleanedParams) 
 *
 * Removes all key-value pairs with keys that are not in Keys from the list of such pairs in
 * OldParams and returns the updated list in CleanedParams.
 *
 * @param Keys: list of constants (entity key names).
 * @param OldParams: list of key-value pairs of the form [key1 = value1, key2 = value2 , ...].
 * @param CleanedParams: list of only those entity key-value pairs with keys not in Keys.
**/
remove(_, [], []).
remove(Keys, [Key = Value | OldParams], [Key = Value | CleanedParams]) :- 
	not(member(Key, Keys)), remove(Keys, OldParams, CleanedParams).
remove(Keys, [Key = _ | OldParams], CleanedParams) :- 
	member(Key, Keys), remove(Keys, OldParams, CleanedParams).