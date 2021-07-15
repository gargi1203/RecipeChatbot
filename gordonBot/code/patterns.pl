%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Patterns (listed alphabetically on their labels/names)     			%%%
%%%										%%%
%%% A pattern is a list that consists of the pattern name or id as first 	%%%
%%% item and a subsequent list of actor/intent pairs which indicate which 	%%%
%%% intents should follow each other and who is expected to have the turn.	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversational Activity Patterns (Moore Ch5)           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% A2 Patterns: Open Request

% Pattern A2.4 - Open request Agent Detail Request (focused on recipe)
% Agent to follow-up with detailed request for purpose of slot filling after partial user request
% Example:
%	(U: how much water do I need? --> not part of pattern but listed here to provide context)
%	A: for which recipe?
%	U: for the pasta 
pattern([a24detailRequest, [agent, detailRequest], [user, recipeRequest]]).

% Generic pattern for slot filling
pattern([slotFill(X), [agent, repeat(X)]]).

% Use pattern a24detailRequest for filling in missing slots for intent requestRecipeQuantity
slotFill(requestRecipeQuantity, a24detailRequest).

% Use pattern a24detailRequest for filling in missing slots for intent requestRecipeQuantity
%slotFill(requestRecipeQuantity, a24detailRequest).

% Pattern A2.5: Open Request (User)
% Example: (focused on clarifying a recipe step)
%	(A: boil a pan full of water. --> added to provide context, not part of pattern)
%	U: how do i that?
%	A: fill your pan until about three-fourth of water and heat until the water boils 
% user can ask the agent to provide with more information on the current step
% agents will give the additional information in return
pattern([a25, [user, requestClarification], [agent, provideClarification]]).

% Pattern A2.5: Open Request (variant asking for ingredient quantity)
% Example:
%	U: what is the amount of water needed for the pasta?
%	A: for the pasta you need about three-fourth of a large pan of water.
% Instruction:
% 1. Add the user intent requestRecipeQuantity and the agent intent grantRecipeQuantity (see text.pl
%	for more information).
% 2. Introduce a pattern a25recipeQuantity here to address user requests about ingredient quantities. 
% User can ask a question about the quantity for an certain ingredient of a recipe and agent responds with the quantity
pattern([a25recipeQuantity, [user, requestRecipeQuantity], [agent, grantRecipeQuantity]]).
% User can ask a question about if a certain utensil is needed for a recipe, agent then responds with yes or no	
pattern([a25recipeUtensil, [user, requestRecipeUtensil], [agent, grantRecipeUtensil]]).

%%% A3 Patterns: Extended Telling
% Pattern A3.0: Extended Telling (Agent; Recipe variant)
% Steps in the 'story' are determined by the recipe; each separate step is treated as a sub-pattern
% that is repeated using the 'repeat' agenda management instruction. 
% agents loops through the steps of the recipe until final step
pattern([a30recipe, [agent, recipeConfirm], [agent, repeat(a30recipeStep)], [agent, finalStep]]).
% agent waits for user to continue with the step -> steps are printed one by one
pattern([a30recipeStep, [agent, recipeStep], [user, recipeContinuer]]).
% when user wants to change recipe while agent explains step, agent restarts
pattern([a30recipeStep, [agent, recipeStep], [user, recipeRequest], [agent, restart]]).


%%% A5 Patterns: Inquiry (Agent)
% Pattern A5.0: Closed Inquiry (Agent; variant asking for a recipe name).
% Example:
%	A: what recipe would you like to cook?
%	U: i'd like to make pasta (or, simply: pasta)
%	A: pasta is a great choice!
% Instruction:
% 1. Add the intents 'recipeInquiry' and 'recipeChoiceReceipt' to the text.pl file so the agent is 
%	able to generate the correct text for these intents. Add the intent 'recipeRequest' to your 
%	Dialogflow agent.
% 2. Add a pattern with pattern id 'a50recipeName' here where the agent initiates this part of 
%	the conversation with an inquiry about the recipe the user would like to cook, the user can 
%	reply with his or her choice, and the agent confirms the choice is available (only) if the 
%	recipe is present in its database (i.e. the cooking.pl file).
% 3. Add the 'a50recipeName' id at the head of the agenda list in the dialog_init.mod2g file and 
%	remove the 'recipe = pasta' in that file from the agent's initial memory.
pattern([a50recipeName, [agent, recipeInquiry], [user, recipeRequest], [agent, recipeChoiceReceipt]]).

% Pattern A5.0: Closed Inquiry (Agent; variant checking for ingredients).
% Example 1:
% 	A: do you have all the ingredients needed?
%		     2 cloves garlic
% 		3 table spoons olive oil
%		     400 grams penne
% 		 50 grams grated cheese
%	   about three-fourth of a large pan water
%	U: yes i do
%	A: that's great!
% Example 2:
%	A: do you have all the ingredients needed?
%		     2 cloves garlic
% 		3 table spoons olive oil
%		     400 grams penne
% 		 50 grams grated cheese
%	   about three-fourth of a large pan water
%	U: no, i don't think so
%	A: i'm sorry to hear that
% Instruction:
% 1. Add an 'ingredientsCheck' intent for the agent to the text.pl file and a 'confirmation' and  
%	a 'disconfirmation' intent for the user to your Dialogflow agent.
% 2. Add two variants of an a50ingredientsCheck pattern. Use the confirmation, disconfirmation, and
%	ingredientsCheck that you introduced and the given positiveReceipt and
%	negativeWelfareReceipt intents for the agent (see text.pl). Also use the agenda instruction
%	'terminate' that you implemented to clear the agent's agenda in variant 2 (which in 
%	combination with the last topic check you implemented earlier should enable a user to start 
%	over again).
% 3. Add the a50ingredientsCheck pattern id to the agent's agenda.
% Ingredients
% user confirms that they have all the ingredients in the list
pattern([a50ingredientsCheck, [agent, ingredientsCheck], [user, confirmation], [agent, positiveReceipt]]).
% user disconfirms that they have all the ingredients in the list, the agent terminates in responds
pattern([a50ingredientsCheck, [agent, ingredientsCheck], [user, disconfirmation], [agent, negativeWelfareReceipt], [agent,terminate]]).

% Utensils
% user confirms that they have all the utensils in the list
pattern([a51utenstilCheck, [agent, utensilsCheck], [user, confirmation], [agent, positiveReceipt]]).
% user disconfirms that they have all the utensils in the list, the agent terminates in responds
pattern([a51utenstilCheck, [agent, utensilsCheck], [user, disconfirmation], [agent, negativeWelfareReceipt], [agent,terminate]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sequence Management Patterns (Moore Ch6)               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% B1 Patterns: Repair (Agent)
% Pattern B1.2: Paraphrase Request (Agent)
% Example:
%	U: chicken soup
%	A: what do you mean?
% Instruction:
% 1. Introduce a 'paraphraseRequest' intent for the agent.
% 2. Add a b12 pattern using the defaultFallback and paraphraseRequest intent.
% user says anything not recognizable for the agent and the agent will ask for 'clarification' from the user
pattern([b12, [user, defaultFallback], [agent, paraphraseRequest]]).

% Pattern B1.3: Out of context dialog move
% For handling intents that are recognized but not expected nor first intent of a pattern.
% Example:
%	A: what recipe would you like to cook?
%	U: next step
%	A: not sure what that means in this context.
% Instruction:
% 1. Add a b13 pattern using the Prolog don't care symbol (_) for the user intent and the given
%	contextMismatch agent intent to respond to the user intent. Make sure to give this pattern 
%	the pattern id b13!!
% 2. Note that by using the don't care symbol any intent recognised will match with the first intent
%	in this pattern. This requires careful handling of this pattern by the dialog engine! The 
%	dialog engine has already been prepared to handle this special case for you.
% user can say anything that the agent might recognize as an intent but is not sure how to use it in the given situation
pattern([b13, [user,_], [agent, contextMismatch]]).

%%% B4: Sequence Closers
% Pattern B4.2: Sequence Closer Appreciation (helped)
% Example:
%	U: thanks
%	A: you're welcome.
% user expresses appreciation, agent acknowledges it
pattern([b42, [user, appreciation], [agent, appreciationReceipt]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversation Management Patterns (Moore Ch7)           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% C1 Patterns: Opening (Agent)
% Pattern C1.0: Opening Greeting (Agent)
% Example:
%	A: hello
%	U: hi
% Instruction:
% 1. Introduce an intent 'greeting' for greeting by adding this intent to your Dialogflow agent (for
%	user recognition) and the text.pl file (for agent text generation).
% 2. Add a pattern with pattern id c10 here where the agent initiates (i.e. starts) greeting and 
%	then the user is expected to greet.
% 3. Add a rule in the dialog generation module in the right place to update the session by adding
%	the c10 pattern that you introduced in step 2 if (and only if) the session (history) is
%	still empty.
pattern([c10, [agent, greeting], [user, greeting]]).

% Pattern C1.1: Opening Self-Identification (Agent)
% Example:
%	A: hello
%	A: I'm Sous-Chef
%	U: hi
% Instruction:
% 1. Re-use the intent 'greeting' that you introduced for the c10 pattern above and introduce 
%	another intent 'selfIdentification' and add this intent to the text.pl file. Use the 
%	agentName/1 predicate and the built-in string_concat/3 predicate to generate the text 
%	associated with the selfIdentification intent.
% 2. Add a pattern with pattern id c11 here where the agent initiates greeting, then identifies
%	itself, and then the user is expected to greet.
% 3. Add a rule in the dialog generation module in the right place to update the session by adding
%	the c11 pattern that you introduced in step 2 if (and only if) the session (history) is
%	still empty and the agent has a name (check using the agentName/1 predicate).
pattern([c11, [agent, greeting], [agent, selfIdentification], [user, greeting]]).

%%% C3 Patterns: Capabilities
% Pattern C3.0: General Capability Check
% Example:
%	U: what can you do?
%	A: At the moment I can converse with you about nothing in particular. I do know how to make 
%		a tasty pasta, risotto, <recipe2shorthand>, <recipe3shorthand>, however, if you are
%		interested.
% Instruction:
% 1. Introduce the intents 'checkCapability' (for the user) and 'describeCapability' (for the
%	agent). Introduce some Prolog code to concatenate the shorthand names for all the recipes 
%	that you introduced (retrieve these using the recipes/1 predicate, see cooking.pl) and add
%	this code to the text.pl file to specify the correct capability description for your agent.
% 2. Add a pattern here where the user initiates the pattern by checking for the agent's capability 
%	with the agent then responding to the request by describing its capabilities.

pattern([c3, [user, checkCapability], [agent, describeCapability]]).

%%% C4 Patterns: Closing
% Pattern C4.0: Last Topic Check (Agent)
% Example 1:
%	A: is there anything else I can help you with?
%	U: bye
%	A: goodbye
% Example 2:
%	A: is there anything else I can help you with?
%	U: yes
%	A: okay!
% Example 3:
%	A: is there anything else I can help you with?
%	U: i don't think so
%	A: have a good day!
% 	U: goodbye
%	A: bye
% Instruction:
% 1. Introduce intents for 'lastTopicCheck', 'acknowledgement', and a 'wellWish' for the agent (i.e.
%	add these to the text.pl file) and the intents 'confirmation' and 'disconfirmation' for the 
%	user (i.e. add these to your Dialogflow agent). Re-use the 'farewell' intent. 
% 2. Add three variants of the last topic check pattern, each with the same c40 pattern id, that 
%	match the three examples above. For the second variant, use the special agent management 
%	instruction 'restart' as an intent the agent should perform at the end of the pattern
%	variant. You do not need to do anything else than use this intent as it has already been
%	implemented for you (see the dialog generation module).
% 3. Add a rule in the dialog generation module in the right place to update the session by adding
%	the c40 pattern you introduced in step 2.
% user says farewell after lastTopicCheck, agents responds with farewell
pattern([c40, [agent, lastTopicCheck], [user, farewell], [agent, farewell]]).
% user confirms after the lastTopicCheck and the agent acknowledges and restarts (reinitializes the agenda)
pattern([c40, [agent, lastTopicCheck], [user, confirmation], [agent, acknowledgement], [agent, restart]]).
% user disconfirms after the lastTopicCheck and agent wishes the user well whereas the user can respond with a farewell
pattern([c40, [agent, lastTopicCheck], [user, disconfirmation], [agent, wellWish], [user, farewell], [agent, farewell]]).
	

% Pattern C4.3: Closing Farewell (Agent)
% Example:
%	A: goodbye
%	U: bye
% Instruction:
% 1. Introduce an intent 'farewell' for saying goodbye by adding this intent to your Dialogflow 
% 	agent (for user recognition) and the text.pl file (for agent text generation).
% 2. Add a pattern here where the agent initiates (i.e. starts) saying goodbye and then the user is 
%	allowed to say goodbye.
% 3. Add a rule in the dialog generation module in the right place to update the session by adding
%	the c43 pattern you introduced in step 2.

pattern([c43, [agent, farewell], [user, farewell]]).

% !!!!!!!!!!!!!!!!!!!!! IMPROVEMENTS !!!!!!!!!!!!!!!!!!!!!!!
% IMPROVEMENT 1
% Pattern A5.2 (addition and improvement on the code) : Origin Check (improvement 1)
%
% Agent asks the user if they want to know the origin of their chosen recipe, user answers yes or no.
% Depending on the answer, either, the agent gives the user the origin or responds with a negativeResponse

% user confirms the origin story of the recipe
pattern([a52OriginCheck,[agent, originQuestion],[user, confirmation], [agent, originCheck], [user, confirmation], [agent, positiveReceipt]]).
% user declines the origin story of the recipe, agent moves on
pattern([a52OriginCheck, [agent, originQuestion], [user, disconfirmation], [agent, negativeResponse]]).

% IMPROVEMENT 2
% Pattern A3.1: optional to add a garnish on top of the serving 
% after the final step, the agent will ask the user if they want to add an additional garnish on top of the serving
% they user can then confirm or decline the offer

% user accepts the offer of a garnish
pattern([a31garnishStep, [agent, garnishRequest], [user, confirmation], [agent, garnishOption]]).
% user declines the offer of a garnish
pattern([a31garnishStep, [agent, garnishRequest], [user, disconfirmation], [agent, negativeResponse]]).

% IMPROVEMENT 3
% agent asks the user which 'role' they would like to play in the cooking, either female or male
% user confirms they are male
%pattern([c12, [agent, genderIdentification], [user, maleChef], [agent, genderMaleConfirmation]]).
% user confirms they are female
%pattern([c12, [agent, genderIdentification], [user, femaleChef], [agent, genderFemaleConfirmation]]).
% has trouble recognizing a female