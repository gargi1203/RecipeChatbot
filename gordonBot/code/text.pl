%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scripted text and phrases for ** GENERIC ** intents (sorted on intent name)		%%%
%%% Text is only provided for those intents that the agent will generate (use). 	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intent : acknowledgement
text(acknowledgement, "Yes Chef!"). 

% Intent: appreciation receipt
text(appreciationReceipt, "You're welcome.").

% Intent: context mismatch
text(contextMismatch, "I am not sure what you mean in this context, Chef.").

% Intent: describe capability
text(describeCapability, Txt) :- recipes(Recipes), atomics_to_string(Recipes,', ', String), 
	string_concat("Let me inspire you with three popular recipes. These are the ones that I can make ", String, Txt).

% Intent: farewell
text(farewell, "Until we cook again!").

% Intent: last topic check
text(lastTopicCheck, "Is there anything else I can help you with?").

%Intent: negative response
text(negativeResponse, "Alright, no problem!.").

% Intent: negative welfare receipt
text(negativeWelfareReceipt, "I'm sorry to hear that.").

% Intent: recipe origin check (improvement 1)
text(originCheck, Txt) :- getOrigin(Origin),  atomics_to_string(Origin, String),
    string_concat("Here is the Origin!</br>", String, Txt).
    
% Intent: origin request (improvement 1)
text(originQuestion, "Chef! I have found out the origin of the wonderful dish you are making! Would you like to know?").

% Intent: paraphrase request
text(paraphraseRequest, "What do you mean, Chef?").

% Intent: positive receipt
text(positiveReceipt, "Great!").

% Intent: provide clarity
text(provideClarification, Txt) :- currentRecipe(Recipe), stepCounter(Cnt), elicit(Recipe, Cnt, Txt). 

% Intent: garnish request (improvement 2)
text(garnishRequest, 'May I suggest a garnish to top it off with, Chef?').

% Intent: garnish option (improvement 2)
text(garnishOption, Txt) :- currentRecipe(Recipe), garnishStep(Recipe, Garnish), string_concat(Garnish, " a suitable option for this dish", Txt).

%Intent: genderIdentification (improvement 3)
%text(genderIdentification, "Which cook will I be helping today? Jamie Oliver (male) or Melissa Leong (female)?").

%Intent: genderConfirmation (improvement 3)
%text(genderMaleConfirmation, Txt) :- userMaleName(Name), string_concat( "So today I'll be working with ",Name, Txt).
%text(genderFemaleConfirmation, Txt) :- userFemaleName(Name), string_concat( "So today I'll be working with ",Name, Txt).

% Intent: greeting
text(greeting, "Good day Chef!").

% Intent: recipe inquiry 
text(recipeInquiry, "Which one of your michelin star dishes would you like to cook chef?").

% Intent: sequence closer
text(sequenceCloser, "Thank you, Chef.").

% Intent: wish well
text(wellWish, "Enjoy your day!").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scripted text and phrases for ** DOMAIN SPECIFIC ** intents (sorted on intent name)	%%%
%%% Text is only provided for those intents that the agent will generate (use). 	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Intent: self identification 
text(selfIdentification, Txt) :- agentName(Name), 
    string_concat("Hello Chef, I am your virtual cooking assistant, my name is ", Name, Txt).

% Intent: detail request recipe quantity 
% This intent is used for slot filling for the user intent: requestRecipeQuantity
% requestRecipeQuantity has two entities: recipe and ingredient (check your Dialogflow agent)
text(detailRequest, Txt) :- not(currentRecipe(_)), Txt = "For what recipe Chef?". 

% Intent: final step (of a recipe)
text(finalStep, Txt) :- currentRecipe(Recipe), finalStep(Recipe, _, Step), string_concat("For the final step: ", Step, Txt).


% Intent: grant recipe quantity
% This intent is used for answering the user intent: requestRecipeQuantity.
% Instruction:
%	Use the predicates currentRecipe/1, mostRecentIngredient/1, and ingredient/3 to construct a
%	string that provides the answer.
text(grantRecipeQuantity, Txt) :- currentRecipe(Recipe), mostRecentIngredient(Ingredient), ingredient(Recipe, Ingredient, Amount), 
	string_concat("The amount you need is ", Amount, Txt).	

text(grantRecipeUtensil, Txt) :- currentRecipe(Recipe), mostRecentUtensil(Utensil), findall(X, utensil(Recipe, X), List),
	member(Utensil, List), Txt = "Yes" ; Txt = "No".
	
% Intent: ingredients check
% Instruction
%	For the current recipe, combine an ingredients check question with a (nicely formatted) list 
%	of all ingredients with the amount needed for that recipe. Add useful helper predicate
%	definitions to the cooking.pl file. Hint 1: You can add "<br />" for adding new lines (on 
%	the chat webpage). Hint 2: Use the atomics_to_string/3 built-in predicate for combining the 
%	ingredients.
text(ingredientsCheck, Txt) :- ingredientList(IngredientsList), atomics_to_string(IngredientsList, '<br />', String),
	string_concat('Chef! Have you asked your sous-chef to gather all the ingredients that you need? <br />', String, Txt).

text(utensilsCheck, Txt) :- utensilsList(UtensilList),  atomics_to_string(UtensilList,'</br>', String),
	string_concat("What about the cooking utensil?</br>", String, Txt).
    	
% Intent: recipe choice receipt
% Instruction:
% 	Collects recipe name provided by user from session history using the given currentRecipe/1 
% 	predicate and checks that the recipe is available in the agent's recipe database using the
% 	given recipes/1 predicate.
text(recipeChoiceReceipt, Txt):- currentRecipe(Recipe), recipes(Recipes), 
	member(Recipe,Recipes), string_concat(Recipe," is a great choice!", Txt).
	
% Intent: recipe confirm
text(recipeConfirm, Txt) :- currentRecipe(Recipe), 
	string_concat("I will now guide you through the process of making ", Recipe, Txt).
		
% Intent: recipe step
% This intent is used as part of a repeated subpattern and therefore uses the stepCounter/1
% predicate to fetch the right step.
text(recipeStep, Txt) :- currentRecipe(Recipe), stepCounter(Cnt), step(Recipe, Cnt, Txt).