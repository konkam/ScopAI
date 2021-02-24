-   [Intro](#intro)
-   [Optimisation du code](#optimisation-du-code)
    -   [scenarios pour mesure de
        performance](#scenarios-pour-mesure-de-performance)
    -   [profiling](#profiling)
-   [Idées de tests pour les fonctions de
    jeu](#idées-de-tests-pour-les-fonctions-de-jeu)
-   [Idées pour l’apprentissage](#idées-pour-lapprentissage)
    -   [Apprentissage des règles](#apprentissage-des-règles)
-   [Distribution des scores](#distribution-des-scores)

    knitr::opts_chunk$set(cache = TRUE)

    library(ScopAI)

Intro
=====

Dans ce documents, des idées en vrac sur ce qu’on pourra faire

Optimisation du code
====================

scenarios pour mesure de performance
------------------------------------

On peut tester différentes approches pour l’énumération des combinaisons
sur un scénario réalise en utilisant:

    library(microbenchmark)
    microbenchmark(RunGame(seed = 1, starting_player = 1, DecisionFunction = ScopAI:::DummyDecision))

    ## Unit: milliseconds
    ##                                                                               expr
    ##  RunGame(seed = 1, starting_player = 1, DecisionFunction = ScopAI:::DummyDecision)
    ##       min       lq    mean   median       uq      max neval
    ##  1.397774 1.598101 1.86987 1.729464 1.936592 4.894424   100

Tant que deux fonctions de décision prennent les mêmes décisions elles
seront évaluées sur la même série de cartes.

profiling
---------

utiliser `profvis` pour identifier les parties du code les plus lentes.

Idées de tests pour les fonctions de jeu
========================================

Pour voir si on n’a pas introduit d’erreur, on pourrait lancer une
batterie de tests a posteriori sur une partie pour voir si elle s’est
déroulée de façon conforme.

Liste informelle de critères de validité d’une partie:  
- bonne durée: une partie doit compter 36 tours de jeu (36 cartes
déposées par un joueur)  
- le nombre total de cartes doit être conservé  
- pas de cartes dupliquées - nombres de cartes positifs dans chaque
compartiment  
- scores raisonnables

Idées pour l’apprentissage
==========================

Remarque générale: il est plus rapide et efficace de partir d’un réseau
de neurones déjà entraîné sur quelque chose.

Apprentissage des règles
------------------------

On peut vouloir apprendre les règles du jeu en apprenant des choix
licites. Pour cela, on peut soit utiliser des parties licites et
chercher à reproduire les mouvements, soit mettre une pénalité forte à
des mouvements interdits dans la fonction de score, de manière à
pénaliser ces derniers

On peut aussi choisir de contraindre les choix aux mouvements autorisés.

Distribution des scores
=======================

Il serait utile d’étudier au moins par une simulation la distribution
des scores pour différentes stratégies, afin de juger de la variabilité
et de la difficulté d’optimiser des décisions en milieu aléatoire
