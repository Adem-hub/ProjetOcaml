# DM programmation interface Ocaml : Cut The Rope

## Préambule

D'abord, il est important qu'avant de lancer le jeu, vous ouvrez une première fois le fichier et y changez la variable `working_path` pour le chemin du dossier où a été dézippé le jeu. Pensez bien à remplacer les `\` par `\\` et d'en ajouter un à la fin comme dans l'exemple.

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/7c2606d0c6c1fed453adc4f634b43d8b3fffc606/Captures%20du%20jeu/Working_Path.PNG)

Nous vous conseillons aussi, si vous avez le temps de consulter le fichier `Reedme_level_creation.txt` pour créer vos propres niveaux (en plus pas besoins de relancer le jeu pour en ajouter un).

## Présentation du jeu

Notre jeu se base sur Cut The Rope, un jeu Android/IOS créer par ZeptoLab. Le principe est simple faire manger un objet (un bonbon dans le jeu d'origine et un hamburger dans le notre) à un dinosaure (ici Marcus).

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/620776c732366b00c2d4711826739d8bb7124e26/Captures%20du%20jeu/Welcome.PNG)

Lors du lancement du jeu, vous deverez attendre un chargement d'environ 1/1.5 minutes, cependant pas d'inquiètude vous pourrez soit vous laisser emporter par le récit de Marcus afin d'en apprendre plus sur la back-story de jeu, ou bien jouer au mini-jeu proposé (ce dernier ce jouant avec les touches 'a' pour aller à gauche et 'e' pour aller à droite et dont le but est de manger le plus de hamburger possible, notre record est de 23). On notera que cela reste un mini-jeu et le chargement prime sur ce dernier, ainsi il freeze en fin de chargement d'un fichier.

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/9cb564be4db2ccb528b6bd9d0445db36bb4f2106/Captures%20du%20jeu/Chargement.PNG)

Ensuite lors de la partie il faudra donner le hamburger à Marcus.

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/a7b1ccfd436a1b1904840773ffd259feedbeaafb/Captures%20du%20jeu/Feed_Marcus.PNG)

Cependant des piques que le hamburger devra éviter vous compliquerons rapidement la tâche.

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/c7ea7e053627fa3e7232d3355b1e467b2ccb884c/Captures%20du%20jeu/Spikes_Example.PNG)

Après votre premier niveau jouer (et même pas besoin de gagner) vous aurez la possibilité d'accèder au menu, par le biais de ce dernier vous aurez accès à plein de niveaux (et même aussi les votre si vous en créer).

![This is an image](https://github.com/Adem-hub/ProjetOcaml/blob/c7ea7e053627fa3e7232d3355b1e467b2ccb884c/Captures%20du%20jeu/Spikes_Example.PNG)

## Développement et enjeux du projet

### La physique

Tout d’abord il fallait simuler la physique des cordes. Afin de se faire, nous avons décidé d’employer une méthode appelée “Verlet’s integration”
(suite à de nombreuses autres idées abandonnées) qui est une alternative à la méthode d’Euler:
[explication de la méthode en détail](https://fr.wikipedia.org/wiki/Int%C3%A9gration_de_Verlet).

### Le stockage des données

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

### L'esthétique

Lors du processus de création l’apparence du code aussi bien que du jeu a été soigné. Ce dernier se divise en section afin que par le nom de la section et de la fonction son utilité soit le plus limpide possible.

Quant à l’apparence du jeu en tant que tel, nous avons voulu rester fidèle au jeu d’origine tout en apportant notre propre touche (l’histoire lors de l’écran de chargement met en place un univers différent avec un problématique similaire). Les écrans de chargement et de transition ont aussi reçu beaucoup d’attention, le premier étant très présent (le jeu met en effet une soixantaine de secondes à se lancer dû aux images à charger, par exemple les fichiers Back.brc et Front.brc qui font tous les deux 800.001 lignes). Ce dernier est donc accompagné d'un mini-jeu et d'un histoire à lire assez simples mais fonctionnels.
