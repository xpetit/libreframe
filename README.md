# LibreFrame Framework

## Introduction

LibreFrame is an Ada framework for GUI softwares and services. See libreframe-doc repository for further informations.

## Copyright

Copyright **Sowebio SARL France** under GPL v3 license.
[https://www.gnu.org/licenses/gpl-3.0.en.html](https://www.gnu.org/licenses/gpl-3.0.en.html).






v22 > LibreFrame

Refactoring v22 > LibreFrame

Le type On_Off a été supprimé. Les paramètres Switch : On_Off ont été remplacés par Enable : Boolean. Les variables privées d'état correspondantes ont été unifiées avec le suffixe Status. Exemple : Debug_On est remplacé par Debug_Status.

Refonte de la présentation des procédures, aussi  bien dans la spec que dans le body :

Avant : function Cfg_Write (Section : String := ""; Parameter : String := ""; Value : String := "";
                       Trailing_Comment : String := "") return Boolean is

Après : function Cfg_Write (Section : String := "";
                            Parameter : String := "";
                            Value : String := "";
                            Trailing_Comment : String := "")
                            return Boolean is



Refonte générale du nommage de l'API

Généralisation d'identifiants plus littéraux que hiérarchiques (i.e. Raise_Exception ou Set_Debug plutôt que Exception_Raise ou Debug_Set)

Généralisation de préfixes permettant de rechercher rapidement des fonctionnalités dans l'API : Get_*, Set_*, Is_*, Has_*, To_*, etc.). Quand c'est pertinent, remplacement des Get_* existants par Is_*


Package Libreframe

L'affichage des exceptions a été amélioré et debuggué.

Pour distinguer les compilateurs CE & PRO des compilateurs FSF qui n'intègrent pas l'affichage des traces avec numéros de lignes, lorsque qu'une exception est produite à partir d'un binaire compilé par GCC FSF, la trace est précédée de la mention : Where possible, addresses have been replaced by line numbers.


Package LOG

Nommé LOG dans le framework v20, ce package avait été renommé MSG dans le framework v22 pour des raisons de compatibilité avec Gnoga, qui contenait déjà un package LOG. LibreFrame n'utilisant plus Gnoga, le nom LOG a été repris.

Le contrôle de l'état de debug a été sorti du package Log pour être réhaussé au niveau du package racine LibreFrame.

Le contrôle de l'affichage de debug a été scindé en deux. D'une part avec la routine Debug_Message pour les messages de log et d'autre part avec la routine Debug_Exception pour les exceptions d'erreur particulières qui nécessiteraient une trace.

Amélioration des logs de debug : Des filtres optionnel de debug peuvent être créés pour limiter les messages de debug. Il n'y a pas, volontairement, de filtre par défaut définis. Compte tenu de la hiérarchie automatiquement définie pour chaque procédure (Framewok_Name.Package_Name.Dubprogram_Name, c'est à dire, par exmple LibreFrame.Sql.Open_Database), on peut soit filtrer sur :

La totalité du framework : Set_Debug_Filter ("LibreFrame")
La totalité d'un package : Set_Debug_Filter ("Sql")
Un sous programme particulier : Set_Debug_Filter ("Sql.Open_Database")
Deux sous programmes particuliers : Set_Debug_Filter ("Sql.Open_Database, Bidule.Sub_Machin")

Ou tout autre combinaison de filtres si nécessaire. De plus, dans les sources d'une application, rien n'empêche de créer de nouveaux filtres puis de les appliquer avec Set_Debug_Filter.


Package SQL

Le package SQL a été simplifié et réécrit :

- Suppression de la dépendance à Gnoga
- Suppression de l'architecture objet
- Suppression de l'instanciation manuelle (dans le code) des bases de données
- Connexion dynamique d'un nombre quelconque de bases de données
- Refactorisation des structures et manipulation uniquement par conteneurs
- Accéder aux bases de données simplement par leur nom ou leur handle
- Compatibilité PostgreSQL
- Compatibilité Firebird (Interbase) 
- Gnoga a servi d'inspiration pour l'interface de bas niveau de SQLite et MySQL.

Fiabilisation de la gestion des bases de données. Un nom de base de donnée déjà connectée ne peut être utilisé pour une nouvelle connexion. Une base de donnée déconnectée libère le nom, qui peut être réutilisé. Le conteneur du schéma de la base de donnée courante, pour les créations et mises à jour, est désormais effacé à chaque ouverture de base de donnée.

Sql.Row_Count was incorrectly implemented for SQLite.

Rapatriement des API debug de LibreFrame vers LibreFrame.Log
Remplacement, quand pertinent, des if...elsif par des case..when
Remplacement des pragmas Import... par des with Import
Renommage des fonctions Ada analogues à celles de l'API C
Suppression Field_Options
Refactoring RS => Record_Set, Field_Descriptions => Get_Columns_Properties, Field_Name => Get_Column_Name, Field_Size => Get_Column_Length (avec amélioration de l'algo pour délivrer les bonnes longueurs décimales, ex (11,2) = 14), Field_Value => Get_Column_Data, Field_Values => Get_All_Columns_Data, Get_Columns_Properties => Get_All_Columns_Properties, Query_ID => Handle, partout quand pertinent Field => Column, DB_* => Database_*

Suppression de use résiduels dispensables.

Chaque binding est documenté par sa page de référence de l'API C de la base de donnée correspondante. Exemple :
      --  https://sqlite.org/c3ref/errcode.html
      function SQLite_Error_Message (sqlite3 : Database_Handle_Type := Database_Handle)
                                     return charbuf_access
                                     with Import, Convention => C, External_Name => "sqlite3_errmsg";




