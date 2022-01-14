# DM programmation interface Ocaml : Cut The Rope

## Préambule

D'abord, il est important qu'avant de lancer le jeu, vous ouvrez une première fois le fichier et y changez la variable 'working_path' pour le chemin du dossier où a été dézippé le jeu. Pensez bien à remplacer les `\` par `\\` et d'en ajouter un à la fin comme dans l'exemple.

![This is an image](https://myoctocat.com/assets/images/base-octocat.svg)

## Physique

Tout d’abord il fallait simuler la physique des cordes. Afin de se faire, nous avons décidé d’employer une méthode appelée “Verlet’s integration”
(suite à de nombreuses autres idées abandonnées) qui est une alternative à la méthode d’Euler:
[explication de la méthode en détail](https://fr.wikipedia.org/wiki/Int%C3%A9gration_de_Verlet).

## Stockage des données

Pour notre projet deux types d’objet on dû être stocké:
des images ou des gifs (sous forme de png sequence)
des niveaux

**1. Gestion des images**

Afin de gérer les images nous avons procédé ainsi:

A l’aide de python, en particulier des modules PIL et numpy, nous avons transformé le format png en des fichiers txt en clair (format propre à notre programme le .brc,
créé avec un programme personnel python assez simple [ici](https://colab.research.google.com/drive/18S-ul2-umBW8ydUtrBednBEv2MOYPWHJ?usp=sharing)).
Les fichier brc se présentent sous le format suivant:

1. La première ligne comporte les dimensions l’image “largeurxhauteur”.

2. Les lignes suivantes contiennent les valeurs RGBA (“r g b a”):
- R (quantité de rouge) de 0 à 255
- G (quantité de vert) de 0 à 255
- B (quantité de bleu) de 0 à 255
- A (transparence) de 0 à 255

Nous avons ensuite mit en place une fonction de lecture de ce format sous Ocaml, qui transforme le fichier en liste de couleur graphics, qui par la suite est interprètée comme une image par la fonction make_image (on notera que cette dernière nécessite la fenêtre graphique ouverte et que l'image crée à un fort taux de corruption lors de la fermeture de la fenêtre).

**2. Gestion des niveau**

Afin de gérer les niveaux, le procédé est très similaire, en effet il s’agit aussi d’un fichier texte avec une nouvelle extension (.niv). Ce dernier est lu afin d’obtenir une structure de plusieurs tableau_dynamiques1 qui correspond aux différents éléments du niveau. (Bien qu’il soit en anglais, le fichier Readme associé au jeu explique comment créer son propre niveau et l’ajouter au jeu).

*note:*

Une autre problématique rencontrée fut la gestion des array, ces derniers étant immuable après création et chaque niveau contenant un nombre variable d’objet (même au cours de la partie le nombre de liens et points varie) il était nécessaire de mettre en place une structure pour gérer les donnés.

Ainsi, le type tableau dynamique est un array à taille variable utilisant 4 méthodes pour fonctionner (on notera que l’array support n’est pas récupérable), soit Tab un tableau de ce type:
- Tab.add valeur,  ajoute la valeur au tableau;
- Tab.id id, regarde le contenue de la case en position id;
- Tab.remove id, retire du tableau la valeur en id;
- Tab.size (), retourne la longueur utilisé du tableau (le tableau étant éventuellement plus long en mémoire).

## Apparence

Lors du processus de création l’apparence du code aussi bien que du jeu a été soigné. Ce dernier se divise en section afin que par le nom de la section et de la fonction son utilité soit le plus limpide possible.

Quant à l’apparence du jeu en tant que tel, nous avons voulu rester fidèle au jeu d’origine tout en apportant notre propre touche (l’histoire lors de l’écran de chargement met en place un univers différent avec un problématique similaire). Les écrans de chargement et de transition ont aussi reçu beaucoup d’attention, le premier étant très présent (le jeu met en effet une soixantaine de secondes à se lancer dû aux images à charger, par exemple les fichiers Back.brc et Front.brc qui font tous les deux 800.001 lignes). Ce dernier est donc accompagné d'un mini-jeu et d'un histoire à lire.
