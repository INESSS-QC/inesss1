**inesss v.0.7.3**

# Support

Pour des questions, des commentaires ou pour désigner tout défaut de
conception, écrire un courriel à l’adresse
`guillaume.boucher@inesss.qc.ca` et indiquer l’objet
`librairie inesss`.  
La documentation de la librairie est disponible
[ici](https://github.com/INESSS-QC/inesss1/tree/master/Documentation)

# Installation de la librairie *inesss*

Copier les lignes suivantes dans la console de *RStudio* pour installer
la librairie *inesss*.

``` r
if (!"remotes" %in% installed.packages()[,1]) install.packages("remotes")
remotes::install_github("INESSS-QC/inesss", upgrade = "never", build_vignettes = T)
```

# Démarrer le formulaire

<img src="Documentation/source/images/formulaire-addin.png" style="width:90.0%" />

<p style="page-break-before: always">
<p style="margin-bottom:3cm">

# Notes de création

## R

R version 4.0.5 (2021-03-31)

## Librairies

`askpass` v.1.1<br> `data.table` v.1.14.0<br> `DBI` v.1.1.1<br> `fs`
v.1.5.0<br> `knitr` v.1.31<br> `lubridate` v.1.7.10<br> `miniUI`
v.0.1.1.1<br> `parallel` v.4.0.5<br> `odbc` v.1.3.1<br> `readxl`
v.1.3.1<br> `rmarkdown` v.2.7<br> `rstudioapi` v.0.13<br> `shiny`
v.1.6.0<br> `shinydashboard` v.0.7.1<br> `shinyFiles` v.0.9.0<br>
`stringr` v.1.4.0<br> `testthat` v.3.0.2<br> `writexl` v.1.3.1

<p style="page-break-before: always">
<p style="margin-bottom:3cm">

# Autres installations/désinstallations

## Installation de R

-   Télécharger la version R 4.0.5 en cliquant
    [ici](https://cloud.r-project.org/bin/windows/base/old/4.0.5/R-4.0.5-win.exe)
    (ou la dernière version
    [ici](https://cloud.r-project.org/bin/windows/base/)).
-   Ouvrir le fichier téléchargé.
-   Choisir la langue utilisée par l’assistant d’installation, puis
    cliquer sur *OK*.
-   **Information** : Une note apparaît indiquant que vous devez avoir
    les privilèges d’administrateurs. Ignorer cet avertissement et
    cliquer sur *Suivant*.
-   **Information** : Une note sur la licence et les droits
    d’utilisation de R apparaît. Cliquer sur *Suivant*.
-   **Dossier de destination** : Choisir un répertoire où vous avez les
    droits d’écriture. Le répertoire par défaut
    `C:\Users\msXXX\Documents\R\R-4.0.5` est un bon choix. Cliquer sur
    *Suivant*.
-   **Composants à installer** : Installer tous les composants. Cliquer
    sur *Suivant*.
-   **Options de démarrage** : Choisir *Non (accepter les valeurs par
    défaut)*.
-   **Sélection du dossier du menu Démarrer** Cliquer sur *Suivant*.
-   **Tâches supplémentaires** : Cliquer sur *Suivant*.
-   **Fin de l’installation** : Cliquer sur Terminer.
-   Ouvrir RStudio (disponible sur les postes de la RAMQ).
-   Cliquer sur *Tools*, puis sur *Global Options…*.  
    <img src="Documentation/source/images/inst-R-options.png" style="width:50.0%" />
-   À la section *R version:*, cliquer sur *Change…*, *Choose a specific
    version of R:*, puis sélectionner la version *\[64-bit\] R-4.0.5*.
    Cliquer sur *OK*.  
    <img src="Documentation/source/images/inst-R-version.png" style="width:37.0%" />
-   Toujours dans les options *General* :  
    <img src="Documentation/source/images/inst-R-global-options.PNG" style="width:40.0%" />
    -   *Save workspace to .RData on exit:*, choisir *Never*.
    -   Décocher *Always save history (even when not saving .RData)*.
-   Dans la section *Code*, puis *Saving*, sélectionner *UTF-8* pour le
    *Default text encoding:*.  
    <img src="Documentation/source/images/inst-R-encoding.PNG" style="width:50.0%" />
-   Redémarrer RStudio pour conserver ces paramètres.
-   Si les paramètres n’ont pas été sauvegardés, supprimer le dossier
    *RStudio-Desktop* au répertoire `C:\Users\msXXX\AppData\Local` où
    *msXXX* est votre numéro d’identifiant. Répéter les étapes à partir
    de *Cliquer sur Tools*.

## Désinstallation de R

Suivre les étapes suivantes pour supprimer une ancienne version de R
installée à partir de la [section 1 *Installation de R*](#inst).

-   Ouvrir le dossier contenant la version de R.  
    Généralement situé au répertoire
    `C:\Users\msXXX\Documents\R\R-4.0.5` où *msXXX* est le numéro
    d’identifiant et *R-4.0.5* doit être remplacé par le numéro de
    version à désinstaller.
-   Cliquer sur le fichier *unins000.exe*.
-   Cliquer sur *Oui* pour désinstaller complètement R ainsi que tous
    ses composants.
-   La désinstallation s’effectue, puis cliquer sur *OK* pour faire
    disparaître le message indiquant que R a été correctement
    désinstallé.
-   *Facultatif* : Le dossier *R-4.0.5*, ou autre numéro de version, est
    toujours existant et contient toutes les librairies qui avaient été
    installées. Pour sauver de l’espace disque, il est conseillé de
    supprimer ce dossier (clic droit de la souris, puis supprimer).

## Rtools40

-   Télécharger *Rtools40* en cliquant
    [ici](https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe).
-   Ouvrir le fichier téléchargé.
-   Conserver les paramètres par défaut en cliquant sur *Next* à chaque
    étape, puis sur *Install*.
-   Cliquer sur *Finish* une fois l’installation terminée.
