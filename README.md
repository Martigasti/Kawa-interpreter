# DM - Kawa 
Auteur: LEIVA Martin 

### Exécution
- Faire 'dune build' pour compiler les fichiers et obtenir le fichier éxécutable 'kawai.exe'.
- Pour éxécuter le programme dans les fichiers de test, taper './kawai.exe tests/nom_fichier.kwa'
- Le fichier principal de tests, qui vérifie tout est test.kwa, donc pour l'éxécuter la ligne serait './kawai.exe tests/test.kwa'

### Fonctionnement du programme
- **Arithmétique**: 
Le programme permet d'utiliser tous les opérateurs demandés dans binop et unop, sauf qu'il y a un petit problème lors de la soustraction de deux nombres. Comme le token '-' est le même tant pour l'opposé comme pour la soustraction, une soustraction doit être écrite avec un espace entre le deuxième nombre et le '-' pour ne pas avoir de problèmes. Par exemple:
// Fonctionne pas           // Fonctionne correctement
print(5-5);                 print(5- 5);
print(5 -5);                print(5 - 5);

- **Variables**:
Dans mon programme, les variables doivent être déclarées et affectées dans des différentes lignes. 
Les variables doivent être déclarées de la forme 'var type id', avec type étant soit un int, bool ou une classe (qui doit exister => être déclarée dans la partie correspondante aux classes).
Les variables globales doivent être déclarées au début du code, pendant que les variables locales doivent être déclarées au début de leurs méthodes correspondantes.
De plus, les variables locales ont priorité aux variables globales, donc si dans une méthode il y a une variable globale ayant un même id qu'une variable locale, alors la variable locale sera celle affectée/utilisée.

- **Instructions**:
Les instructions suivent les consignes demandés dans le sujet. Cela veut dire que if aura toujours besoin d'un else et un booléen comme condition pour fonctionner correctement. La boucle while aura aussi besoin d'un booléen comme condition.

- **Classes et attributs**:
Les classes doivent être déclarées en deuxième dans le code, juste après les variables globales. Celles-ci sont déclarées de la forme 'class id {...}' et ses attributs sont déclarées de la forme 'attribute type id;'. 

- **Méthodes**:
Les méthodes doivent être déclarées dedans leurs classes correspondantes, de la forme 'method type id(...) {}'. Si dans une méthode son type est void, alors elle ne doit pas avoir un return et viceversa. Les méthodes déclarées comme 'method void constructor...' sont utilisées lorqu'on affecte une nouvelle instance, par exemple 'p = new point(1,1)'. Ainsi, pour créer un nouvel objet d'une classe, il y a deux façons (supposons que la classe point avec x et y existe):
// Pas de constructeur nécessaire   // Constructeur nécessaire
p = new point;                      p = new point(1,1);
p.x = 1;
p.y = 1;

Sur les méthodes, j'ai une particuliarité lorsque j'essaye de faire un return dans une boucle while. Si le return est écrit qu'à l'intérieur d'une boucle while, le programmera ne compilera pas. Cela est dû à une fonction qui regarde si une méthode fait un return (fin du typechecker.ml). Ici, j'ai écris un code de la forme que lorsque le programme lit une ligne while, il ne vérifiera pas son interieur, en renvoyant false directement. Cela cause qu'une méthode comme la suivante, renvoie une erreur disant que pas tous ses 'chemins' ont un return:
    // Ne compile pas
    method int test_while() {
      var int i, n;
      i = 4;
      n = 1;
      while (i > 1) {
        n = n * i;
        i = i - 1;
        return n;
      }

- **Héritage**:
Pour l'héritage, j'ai respecté les consignes du sujet. Les classes héritées doivent être déclarées de la forme:
'class id extends id_parent {...}'. Cela cause que la classe id aura les attributs et méthodes de la classe id_parent, plus le code écrit dedans cemme-ci.  


### Erreur extra
- **Commentaires '//' dedans '/* */'** : En faisant mon ficher final de tests, je me suis rendu compte que lorsque j'essaye d'écrire des commentaires '//' dedans un code commenté dans des '/**/', la partie qui devrait être commenté après les '//' cause des erreurs, par exemple:
        /* method void thing() { 
        return 1;      // Ne doit pas compiler
        } */
cause une erreur: lexical error: unknown character : N.
Or, le code suivant compile correctement:
        /* Erreurs que le typechecker devrait renvoyer */
        // Ne doit pas compiler
        // attend un void, reçoit un int
        /* method void thing() { 
        return 1; 
        } */

### Difficultés Rencontrées

- **Définition des classes** : J'ai eu des grandes difficultés pour que ma partie des classes fonctionne correctement. Le problème principal n'a pas été le code du typechecker ni du interpreter, mais plutot un simple erreur dans le kawaparser. Sur la règle program, j'avais mis en premier les classes et en deuxième les variables globales, ce qui provocait une erreur. Pour fixer cet erreur j'ai simplement inversé ces deux éléments, mais pour le remarquer j'ai perdu trop de temps.

- **Création des méthodes** : Je pense que cette partie a été la plus compliquée du projet, dû à la grande quantité de vérifications nécéssaires dans le typechecker. Je n'ai pas eu aucune difficulté ou blocage spécifique, mais ce qui était dur c'était à penser dans tous les tests et erreurs possibles que le programme pourrait avoir.


### Extensions Traitées

- **Déclarations en série** : Ajouter la possibilité de déclarer simultanément plusieurs variables du même type, sous la forme: 'var int x, y, z;'.     
Pour l'implementer, j'ai seulement modifié le fichier kawaparser.mly. Sur ce fichier, j'ai crée une nouvelle règle de grammaire appelée ident_list qui permet de reconnaître une liste d'identifiants séparés par des virgules. Cette liste est ensuite utilisée dans la règle var_decl pour associer chaque identifiant à son type respectif. De plus, sur la règle global_vars et sur locals (des méthodes), j'ai ajouté List.flatten pour éliminer la liste 'extra' des paramètres déclarés en série et mettre tout sur une liste de dimension 1.


- **Égalité structurelle** : Ajouter au langage un opérateur === d'égalité structurelle et sa négation =/=. L'égalité structurelle répond aux critères suivants :
        - chaque constante est égale à elle-même,
        - deux objets sont égaux si et seulement si ils sont des instances de la même classe et ont pour chaque champ des valeurs structurellement égales,
        - des valeurs de types différents ne sont pas comparables.
Pour l'implementer, j'ai modifié divers fichiers:
        - **kawalexer.mll**: J'ai ajouté les tokens STREQ (===) et STRNEQ (=/=).
        - **kawaparser.mly**: J'ai ajouté ces tokens dans les règles de grammaire sous Binop, et définis les priorités pour ces opérateurs.
        - **typechecker.ml**: J'ai ajouté des tests dans la partie Binop (dans type_expr) pour vérifier que les deux types en paramètre soient égals (même les types des classes).  
        - **interpreter.ml**: J'ai implementé la logique d'évaluation des deux opérateurs, en utilisant une fonction auxiliaire (struct_eq v1 v2) déclarée au début de eval_binop.
        - **kawa.ml**: J'ai ajouté dans le type binop, les cas StrEq et StrNeq. 