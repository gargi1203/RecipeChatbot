%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Domain specific cooking assistant knowledge						%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * currentRecipe(-RecipeDown)
 *
 * Retrieves the current recipe from memory (this assumes that the last time a recipe is mentioned 
 * also is the user's current choice.
 *
 * @param RecipeDown Recipe name in lower case.
**/
currentRecipe(RecipeDown) :- keyValue(recipe, Recipe), downcase_atom(Recipe, RecipeDown).

/**
 * mostRecentIngredient(-Ingredient)
 *
 * Retrieves the latest (mentioned or relevant = updated) ingredient from memory.
 *
 * @param Ingredient The ingredient currently stored in the agent's memory.
**/
mostRecentIngredient(Ingredient) :- keyValue(ingredient, Ingredient).

/**
 * mostRecentUtensil(-Utensil)
 *
 * Retrieves the latest (mentioned or relevant = updated) utensil from memory.
 *
 * @param Utensil The ingredient currently stored in the agent's memory.
**/
mostRecentUtensil(Utensil) :- keyValue(utensil, Utensil).

/**
 * recipes(-Recipes)
 *
 * Collects all (shorthand) recipe names from the recipe database using the recipeName/1 and 
 * shorthandName/2 predicates (both need to be present for a recipe to be retrieved!).
 *
 * @param Recipes A list of (shorthand) recipe names available in this file.
**/
recipes(Recipes) :- setof(RecipeName, Name^(recipeName(Name), shorthandName(Name, RecipeName)), Recipes).

/**
 * steps(a30recipeStep, -Cnt)
 *
 * Computes the number of steps in the current recipe. Assumes that the recipe is a linear structure
 * with steps that are consecutively numbered (i.e., 1, 2, ...).
 *
 * @param Cnt The number of steps in the current recipe (based on the step/3 predicate, excluding
 *		the finalStep/3).
**/
steps(a30recipeStep, Cnt) :- currentRecipe(Recipe), bagof(Nr, Txt^step(Recipe, Nr, Txt), Nrs), max_list(Nrs, Cnt).

/** 
 * getIngredient(List)
 *
 * concatenate the ingredients and its amounts
 *
 * ingredientList(List)
 * 
 * creates a list of the concatenated ingredients and its amounts
**/
% retrieves the ingredient and its amount
getIngredient(List) :- currentRecipe(Recipe), ingredient(Recipe, Ingredient, Amount), string_concat(Ingredient, Amount, List).
% creates a list of the retrieved ingredient and its amount
ingredientList(List) :- findall(X, getIngredient(X), List).

/** 
 * ingredientQuantity(List)
 *
 * retrieves a list of all the utensils from the latest mentioned recipe
**/
utensilsList(List) :- currentRecipe(Recipe), findall(Utensil,utensil(Recipe, Utensil),List).

/** 
 * getOrigin(List)
 *
 * Returns a description of the origin of the recipe
**/
getOrigin(Description) :- currentRecipe(Recipe), findall(X, origin(Recipe, X), Description).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Recipe knowledge									%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * ingredient(Recipe, Ingredient, Amount).
 *
 * Represents the amount needed of an ingredient to cook a recipe for a number of servings.
 *
 * @param Recipe The recipe name .
 * @param Ingredient The ingredient's name.
 * @param Amount The amount needed of the ingredient (a string).  
**/


%%% Recipe 1
% Source: https://tasty.co/recipe/garlic-parmesan-roasted-broccoli
% Name:
recipeName('Garlic Parmesan roasted Broccoli').
% Shorthand name:
shorthandName('Garlic Parmesan roasted Broccoli', 'broccoli').
% Origin recipe : origin(Recipe, Description)
origin('broccoli','Broccoli, botanically known as Brassica oleracea italica, is native to the Mediterranean.
	It was engineered from a cabbage relative by the Etruscansan ancient Italian civilization who lived in what is now Tuscany.').
% Ingredients:
ingredient('broccoli', 'broccoli', ' 3 heads').
ingredient('broccoli', 'oil', ' 2 tablespoons').
ingredient('broccoli', 'lemon juice', ' 1 tablespoons').
ingredient('broccoli', 'vegetarian parmesan cheese', ' ¼ cup (25 g)').
ingredient('broccoli', 'garlic powder', ' 2 teaspoons').
ingredient('broccoli', 'red pepper flakes', ' ½ teaspoon').
ingredient('broccoli', 'salt', ' 1 teaspoon').
ingredient('broccoli', 'pepper', ' 1 teaspoon').

%Utensil
utensil('broccoli','teaspoon').
utensil('broccoli','(big) tablespoon').
utensil('broccoli','spatula').
utensil('broccoli','knife').
utensil('broccoli','cup').
utensil('broccoli','cutting board').
utensil('broccoli','pan').
utensil('broccoli','oven').

% Steps:
step('broccoli', 1, 'Pre-heat oven to 400°F (200°C)').
step('broccoli', 2, 'Separate crowns of 3 Broccoli heads').
step('broccoli', 3, 'Snip off any leaves of the 3 Broccoli heads').
step('broccoli', 4, 'Use a baking sheet and pan for the oven').
step('broccoli', 5, 'Add the broccoli heads').
step('broccoli', 6, 'Add 2 of tbsp oil').
step('broccoli', 7, 'Add ¼ cup (25 g) of vegetarian parmesan cheese').
step('broccoli', 8, 'Add 2 teaspoons of garlic powder').
step('broccoli', 9, 'Add ½ teaspoon of red pepper flakes').
step('broccoli', 10, 'Add 1 teaspoon of salt').
step('broccoli', 11, 'Add 1 teaspoon of pepper').
step('broccoli', 12, 'Mix everything evenly').
step('broccoli', 13, 'Place baking pan in the oven for 35 minutes').
finalStep('broccoli', 14, 'Remove and cool it for 5 minutes').
% Garnish the dish garnish(Recipe, Type of garnish)
garnishStep('broccoli', 'More cheese is').

% elicit(Recipe, StepNr, Info) : used to give additional information about the step
elicit('broccoli', 1, 'Turn on the oven and set the temperature to 400°F (200°C) to pre-heat').
elicit('broccoli', 2, 'Use a knife and cutting board to cut the crowns of 3 heads of the broccoli').
elicit('broccoli', 3, 'Use a knife and cutting board to get rid of any leaves on the on the broccoli heads').
elicit('broccoli', 4, 'Place a baking sheet in a pan to later put in the oven').
elicit('broccoli', 5, 'Place the broccoli heads in the pan with the baking sheet').
elicit('broccoli', 6, 'Pour two tablespoons of oil evenly on the broccoli').
elicit('broccoli', 7, 'Sprinkle ¼ cup (25 g) of vegetarian parmesan cheese over the broccoli so it is evenly spread').
elicit('broccoli', 8, 'Sprinkle the 2 teaspoons evenly on top of the broccoli heads').
elicit('broccoli', 9, 'Sprinkle ½ teaspoon of red pepper flakes over the broccoli').
elicit('broccoli', 10, 'Add the (preferred amount or) 1 teaspoon of salt to the broccoli').
elicit('broccoli', 11, 'Add the (preferred amount or) 1 teaspoon of pepper to the broccoli').
elicit('broccoli', 12, 'Use a spatula or big spoon to mix all the ingredients together in the pan').
elicit('broccoli', 13, 'Open the oven and place the pan in the oven; wait for 35 minutes').
elicit('broccoli', 14, 'Remove the pan out of the oven and place it on the counter, let it cool for 5 minutes before serving').

%%% Recipe 2
% Source: https://tasty.co/recipe/one-pot-chinese-fried-rice
% Name:
recipeName('One-pot Chinese fried rice').
% Shorthand name:
shorthandName('One-pot Chinese fried rice', 'rice').
% Origin recipe : origin(Recipe, Description)
origin('rice','Fried rice first developed during the Sui Dynasty in China and as such all fried rice dishes can trace their origins to Chinese fried rice.
	Many varieties of fried rice have their own specific list of ingredients. In Greater China common varieties include Yangzhou fried rice 
	and Hokkien fried rice').
% Ingredients:
ingredient('rice', 'carrot', ' 1 cup (100 g)').
ingredient('rice', 'green peas', ' 1 cup (100 g)').
ingredient('rice', 'garlic', ' 6 cloves').
ingredient('rice', 'shrimp', ' ½ lb (250 g)').
ingredient('rice', 'ham', ' ½ lb (250 g)').
ingredient('rice', 'rice', ' 2 ½ cups (500 g)').
ingredient('rice', 'soy sauce', ' 5 tablepoons').

% Utensil
utensil('rice','tablespoon').
utensil('rice','fork').
utensil('rice','spatula').
utensil('rice','cup').
utensil('rice','knife').
utensil('rice','cutting board').
utensil('rice','small pan').
utensil('rice','large pan').
utensil('rice','stove').

% Steps:
step('rice', 1, 'Chop 1 cup (100 g) of carrot').
step('rice', 2, 'Prepare a large pan at medium heat on the stove').
step('rice', 3, 'Add some oil to the pan').
step('rice', 4, 'Add the chopped carrot').
step('rice', 5, 'Add 1 cup (100 g) of green peas').
step('rice', 6, 'Stir the ingredients in the pan until the vegetables begin to soften').
step('rice', 7, 'Chop 6 cloves of garlic').
step('rice', 8, 'Peel and devein ½ lb (250 g) of shrimp').
step('rice', 9, 'Chop ½ lb (200g) of ham in squares').
step('rice', 10, 'Add the garlic to the pan').
step('rice', 11, 'Add the shrimp to the pan').
step('rice', 12, 'Add the ham to the pan').
step('rice', 13, 'Cook everything for 2 minutes').
step('rice', 14, 'Cook 2 ½ cups (500 g) of rice').
step('rice', 15, 'Beat 3 eggs in a bowl').
step('rice', 16, 'Add rice and eggs to the pan').
step('rice', 17, 'Add 5 tablespoons of soy sauce').
finalStep('rice', 18, 'Mix everything').
% Garnish the dish garnish(Recipe, Type of garnish)
garnishStep('rice', 'Some sprinkled sesame seeds are').

% elicit(Recipe, StepNr, Info) : used to give additional information about the step
elicit('rice', 1, 'Use a knife and cutting board to chop 1 cup (100 g) of carrot in small cubes').
elicit('rice', 2, 'Place a large pan on the stove and turn the heat on medium as preparations').
elicit('rice', 3, 'Pour a preferred amount of oil in the large pan').
elicit('rice', 4, 'Add the chopped carrots to the large pan').
elicit('rice', 5, 'Add the 1 cup (100 g) of green peas to the large pan').
elicit('rice', 6, 'Use a spatula to stir the ingredients in the large pan until they feel soft').
elicit('rice', 7, 'Use a knife and cutting board to cut 6 cloves of garlic in tiny pieces').
elicit('rice', 8, 'To peel ½ lb (250 g) shrimp is to remove the shell from the shrmip <br />
	To devein ½ lb (250 g) shrimp is to remove the black intestine in the middle of the back <br />').
elicit('rice', 9, 'Use a knife and cutting board to chop ½ lb (200g) of the ham in small cubes').
elicit('rice', 10, 'Place the chopped garlic in the large heating pan').
elicit('rice', 11, 'Add the peeled and deveined shrimp to the ingredients in the large heating pan').
elicit('rice', 12, 'Add the small cubes of ham into the heating pan').
elicit('rice', 13, 'Let the ingredients cook in the pan for 2 minutes; occasionally stir the ingredients with a spatula').
elicit('rice', 14, 'Cook 2 ½ cups (500 g) of rice in a separate small pan to the according to the given instructions (the package)').
elicit('rice', 15, 'Break 3 eggs in a small bowl and beat them until the egg yolk is mixed with the egg white').
elicit('rice', 16, 'Add the cooked rice and beaten eggs to the large pan with the rest of the ingredients').
elicit('rice', 17, 'Pour the 5 tablespoons of soy sauce on top of the ingredients').
elicit('rice', 18, 'Mix everything evenly before serving').
%%% Recipe 3
% Source: https://tasty.co/recipe/slow-cooker-mixed-berry-cobbler
% Name:
recipeName('Slow-cooker mixed berry cobbler').
% Shorthand name:
shorthandName('Slow-cooker mixed berry cobbler', 'cobbler').
% Origin recipe : origin(Recipe, Description)
origin('cobbler','Cobblers originated in the British American colonies. English settlers were unable to make traditional suet puddings 
	due to lack of suitable ingredients and cooking equipment so instead covered a stewed filling with a layer of uncooked plain biscuits, scone batter or dumplings, fitted together.').
% Ingredients:
ingredient('cobbler', 'raspberry', ' 1 cup (125 g)').
ingredient('cobbler', 'blueberry', ' 1 cup (100 g)').
ingredient('cobbler', 'strawberry', ' 1 cup (100 g)').
ingredient('cobbler', 'blackberry', ' 1 cup (100 g)').
ingredient('cobbler', 'cornstarch', ' 1 tablespoon').
ingredient('cobbler', 'vanilla cake mix', ' 1 package').
ingredient('cobbler', 'butter', ' 8 tablepoons').

% Utensil
utensil('cobbler','tablespoon').
utensil('cobbler','spatula').
utensil('cobbler','knife').
utensil('cobbler','cup').
utensil('cobbler','cutting board').
utensil('cobbler','slow-cooker').
utensil('cobbler','stove').

% Steps:
step('cobbler', 1, 'Put 1 cup (125 g) of raspberries into the bottom of a slow-cooker').
step('cobbler', 2, 'Add 1 cup (100 g) of blueberries').
step('cobbler', 3, 'Add 1 cup (100 g) of strawberries').
step('cobbler', 4, 'Add 1 cup (110 g) of blackberries').
step('cobbler', 5, 'Sprinkle the cornstarch on top').
step('cobbler', 6, 'Mix everything gently together').
step('cobbler', 7, 'Pour vanilla cake mix on top of the berries and spread out so it covers the berries').
step('cobbler', 8, 'Cut 8 tablespoons of butter into pieces').
step('cobbler', 9, 'Place pieces of butter on top of the cake mix').
step('cobbler', 10, 'Cover the slow cooker with a lid').
finalStep('cobbler', 11, 'Cook on high for 2½ to 3 hours, until the cake is fully cooked').
% Garnish the dish garnish(Recipe, Type of garnish)
garnishStep('cobbler', 'A scoop of ice cream is').

% elicit(Recipe, StepNr, Info) : used to give additional information about the step
elicit('cobbler', 1, 'Place the 1 cup (125 g) of raspberries in the slow-cooker at the bottom').
elicit('cobbler', 2, 'Add the 1 cup (100 g) of blueberries to the raspberries in the slow-cooker').
elicit('cobbler', 3, 'Add the 1 cup (100 g) of strawberries to the raspberries and blueberries in the slow-cooker').
elicit('cobbler', 4, 'Add the 1 cup (100 g) of blackberries to the raspberries, blueberries, and strawberries in the slow-cooker').
elicit('cobbler', 5, 'Sprinkle the 1 tablespoon of cornstarch on top of all the berries in the slow-cooker').
elicit('cobbler', 6, 'Use a spatula to mix the berries and cornstarch gently in the slow-cooker').
elicit('cobbler', 7, 'Pour the one package of vanilla cake mix evenly over the berries, so the cake mix covers them').
elicit('cobbler', 8, 'Use a knife and a cutting board to cut the 8 tablespoons of butter in small pieces').
elicit('cobbler', 9, 'Place the cut pieces of butter on top of the cake mix in the slow-cooker').
elicit('cobbler', 10, 'Use a lid to cover the slow-cooker').
elicit('cobbler', 11, 'Place the slow-cooker on the stove with high heat and let it cook for 2½ to 3 hours; until the cake is fully cooked').
elicit('cobbler', 12, 'When making a serving place ice cream on top').
