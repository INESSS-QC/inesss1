Librairie [`inesss` v.0.5.0](https://github.com/INESSS-QC/inesss1)
------------------------------------------------------------------

### v0.5.0

-   Fonction `SQL_comorbidity`.
-   Fonction `SQL_comorbidity_diagn`.
-   Fonction `SQL_obstetric`.
-   Fonction `comorbidity`.
-   Data `Comorbidity_diagn_codes`.
-   Data `Charlson_diagn_codes`.
-   Data `Elixhauser_diagn_codes`.
-   Data `Comorbidity_weights`.
-   Data `Obstetrics_diagn_codes`.

#### [v0.4.0](https://github.com/INESSS-QC/inesss1/pull/7)

-   `query_stat_gen1()` remplace `stat_gen1_query()`.
-   `SQL_stat_gen1()` utilise l’argument *group\_by* au lieu de
    *result\_by*. `group_by='Codes'` au lieu de `'Périodes'`.
-   Mise à jour du fichier *AIDE-FORMULAIRE-EXEMPLES-ARGUMENTS.xlsx*.
-   Mise à jour du fichier *AIDE-FORMULAIRE-EXEMPLES-RÉSULTATS.xlsx*.
-   Data `V_DEM_PAIMT_MED_CM.SMED_COD_DIN`.
-   Data `V_DEM_PAIMT_MED_CM.SMED_COD_SERV`.
-   Data `V_DENOM_COMNE_MED`.
-   Data `V_DES_COD`.
-   Data `V_PRODU_MED.NOM_MARQ_COMRC`.

### [v0.3.0](https://github.com/INESSS-QC/inesss1/pull/6)

-   Le formulaire, section *Requêtes via Excel*, indique le répertoire
    du fichier sélectionné.
-   Statistiques générales (`stat_gen1`) permet d’afficher les résultats
    par `Teneur` et `Format`.
-   Mise à jour de la documentation : 2021-01-14.

### [v0.2.1](https://github.com/INESSS-QC/inesss1/pull/5)

-   Nouvelle interface du formulaire, section *Statistiques générales*
    : 1) Diviser en trois section distinctes; 2) Le code SQL s’affiche
    en même temps que le tableau des résultats; 3) Bouton qui
    réinitialise les arguments.

### [v0.2.0](https://github.com/INESSS-QC/inesss1/pull/4)

-   Fonction `formulaire()`.
-   Fonction `sql_connexion()`.
-   Fonction `sql_stat_gen1()`.
-   Création du fichier *AIDE-FORMULAIRE\_2021-01-06.pdf*.
-   Création du fichier *LISEZ-MOI\_2021-01-06.pdf*.
-   Création du fichier *R\_RTOOLS\_INSTALLATION\_2021-01-06.pdf*.
-   Exemples au format Excel. Voir fichiers
    *AIDE-FORMULAIRE-EXEMPLES-ARGUMENTS.xlsx* et
    *AIDE-FORMULAIRES-EXEMPLES-RÉSULTATS.xlsx*.
-   Démarrage du formulaire via les *Addins* de *RStudio*.

### v0.1.1.9000

-   Documentation des datas *Pop\_QC*, *RLS\_convert* et *RLS\_list*.
