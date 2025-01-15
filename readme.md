# Kawa interpreter project

## Ce qui fonctionne
Normalement toute partie obligatoire marche

## Difficultés rencontrées
- Colonne par colonne ne permettait pas de vérifier au fur et à mesure, comme indiqué dans les instructions. Hereusement cela n'a pas posé trop de problèmes et permettait de traiter chaque partie de l'interprète plus ou moins séparément, ce que j'ai apprécié. Cependant, la prochaine fois je ne risquerai pas cette démarche.
- La gestion des variables locales/globales ainsi que les variables/methodes obtenu par héritage était d'abord un peu oublié. Ce n'était pas trop difficile de rajouter plus tard, par contre l'organisation du code n'est pas la plus jolie.
- Pour les initialisations en declaration il fallait utiliser les
fonctions d'evaluation en dehors d'exec_seq, mais c'était difficile et pas joli d'extraire ces fonctions.
Ainsi la fonction init_vars pas trop efficace donne pourtant une solution plutôt élégante. 
- Avec le syntaxe de tableau implementé en premier, après l'élimination des mots-clés var, method et attribute, il y avait une ambiguïté très difficile à traiter de manière satisfaisante avec un parser LR(1). La declaration d'un tableau d'instances d'une classe "foo" et l'indexation d'une variable nommé "foo" était identique. 
Au moment où le parser devrait choisir entre ces deux choix, il n'y avait pas encore assez d'information pour décider. J'ai donc introduit un syntaxe spécifique pour les tableaux de classes défini dans les méthodes: @foo[2] est donc une déclaration (tableau de foo) et foo[2] (foo est un tableau) un accès mémoire.

## Choix
- Ne te laisse pas ignorer une valeur de retour.
- Suggère des variables de nom similaire si variable inexistante est utilisée
- Tableaux implementés
- Surcharge statique implementé (et message d'erreur donnant les listes d'arguments si methode pas trouvé)
- Declarations initiales implementé
- Pour l'insant, ne montre que la localisation d'une erreur de typage si cela concerne un opérateur binaire. Une autre branche WIP est utilisé pour implémenter des localisations plus ambitieuses.
- Déclarations multiple de même signature pas autorisées

## Extensions
- Déclarations simplifiées
- Déclaration avec valeur initiale
- Tableaux
- Surcharge statique
- Localisation d'erreur (seulement sur les opérateurs binaires)
- Suggestion d'un identifiant existant, si le programme tente d'acceder à un identifiant inexistant

## Todo
- Remove all asserts 