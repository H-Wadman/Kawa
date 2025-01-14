# Kawa interpreter project

## Difficultés rencontrées
- Colonne par colonne ne permettait pas de vérifier au fur et à mesure. Hereusement cela n'a pas posé trop de problème
- La gestion des variables locales/globales ainsi que les variables/methodes obtenu par héritage était d'abord un peu oublié. Ce n'était pas trop difficile de rajouter plus tard, par contre l'organisation du code n'est pas la plus jolie.
- Pour les initialisations en declaration il fallait utiliser les
fonctions d'evaluation en dehors d'exec_seq, mais c'était difficile et pas joli d'extraire ces fonctions.
Ainsi la fonction init_vars pas trop efficace donne pourtant une solution plutôt élégante. 

## Choix
- À la fin, j'ai décidé d'extraire l'implementation de pas mal des fonctions 
dans typechecker.ml, car je trouvais cela difficile de lire. Je ne suis pas sûr que ce soit mieux, j'aurais peut-être dû nettoyer encore un peu, ou bien j'aurais dû ne rien faire. Bon, c'est comme ça.
J'ai gardé la fonction "check" qui est la seule utilisé dans la suite de la 
fonction "typecheck_prog"
- Ne te laisse pas ignorer une valeur de retour.
- Suggère des variables de nom similaire si variable inexistante est utilisée
- Tableaux implementés
- Surcharge statique implementé (et message d'erreur donnant les listes d'arguments si methode pas trouvé)
- Declarations initiales implementé
- Pour l'insant, ne montre que la localisation d'une erreur de typage si cela concerne un opérateur binaire. Une autre branche WIP est utilisé pour implémenter des localisations plus ambitieuses.
- Déclarations multiple de même signature pas autorisées

## Todo
- Remove all asserts 