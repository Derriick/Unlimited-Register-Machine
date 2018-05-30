# TP noté de Modèles de calcul - Unlimited Register Machine
## Utilisation
L'ensemble des fichiers de l'URM se trouve dans le dossier `src/` du projet. Un fichier Makefile est disponible afin de compiler le programme pour ensuite pouvoir l'exécuter :
```
cd src/
make
./app
```
L'ensemble des tests se lance alors et leurs résultats sont affichés sur le terminal.
## Ajout d'un test
Pour ajouter puis exécuter un programme de test, il suffit d'éditer le fichier `src/app.ml`, écrire le programme sous la forme d'un tableau d'instructions et de l'exécuter à l'aide de la fonction `execX` appropriée (X étant le nombre de registres modifiés, actuellement compris entre 1 et 4).
Il est également possible d'ajouter des expressions et de les tester en utilisant la fonction `expr_to_prog` puis en exécutant les programmes générés avec `execX`.
Après ces modiffications, il est nécessaire de recompiler le projet.
