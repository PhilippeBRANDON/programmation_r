# Cours de Programmation R

---
 
  | STID 1 - Programmation Statistique  
  | TD1  
  | GitHub et introduction à R
author: "Anthony SARDELLITTI"
output: 
  pdf_document:
    toc: true
    number_sections: true
    toc_depth: 3
date: "2023-01-01"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



# Utiliser GitHub

## Qu'est ce que Git

Un peu de lecture [ici](https://www.atlassian.com/fr/git/tutorials/what-is-git). 

## Création d'un repository

1. Créer un compte personnel sur GitHub
Vous utiliserez ce compte pour tous les cours de programmation statistique.
2. Créer un repository nommé *programmation_r* avec un fichier **README.md**
* [lien utile : bonnes pratiques](https://docs.github.com/fr/repositories/creating-and-managing-repositories/best-practices-for-repositories). Vous pouvez créer un repository public ou privé.
* [lien utile : créer un repository](https://docs.github.com/fr/repositories/creating-and-managing-repositories/creating-a-new-repository)
3. Créer une branche appelée *td1*
* [lien utile :  créer une branche](https://docs.github.com/fr/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-and-deleting-branches-within-your-repository)

## Utiliser GitHub

### Option 1 : Avec GitHub Desktop (NOOB)

1. [Installer GitHub Desktop](https://desktop.github.com/)
2. Un peu de lecture pour la suite [ici](https://docs.github.com/fr/desktop/installing-and-configuring-github-desktop/overview/getting-started-with-github-desktop)
3. Cloner votre repository *programmation_r* sur votre machine
4. Connecter à la branche *td1*
5. Ajouter une modification à votre fichier **README.md**
6. Faire un commit de cette modification sur votre branche *td1*
7. Pousser le commit vers votre repository distant
8. Vérifier si votre repository distant à pris en compte cette modification sur votre branche *td1*

### Option 2 : Avec GitBash (GOAT)

#### Configurer GitBash

1. Installer Git
2. Installer GitBash
3. Configurer la connexion SSH à votre repository distant
* [lien utile : connexion SSH](https://linuxhint.com/clone-repo-with-ssh-key-in-git/)
4. Cloner votre repository *programmation_r* sur votre machine

#### Utiliser GitHub avec GitBash

0. Un peu de lecture [ici](https://www.jesuisundev.com/comprendre-git-en-7-minutes/).
1. Connecter à la branche *td1*
2. Ajouter une modification à votre fichier **README.md**
3. Faire un commit de cette modification sur votre branche *td1*
4. Pousser le commit vers votre repository distant
5. Vérifier si votre repository distant à pris en compte cette modification sur votre branche *td1*

## Effectuer une pull request

Depuis votre compte GitHub web.

1. Effectuer une pull request de votre branche *td1* sur votre branch par défaut (*main* ou *master*).
* [lien utile : tutoriel pull request](https://docs.github.com/fr/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)
2. Effectuer un merge de vos deux branches à partir de votre pull request précédente.
* [lien utile : tutoriel merge request](https://docs.github.com/fr/pull-requests/collaborating-with-pull-requests/incorporating-changes-from-a-pull-request/merging-a-pull-request)

# Les bases de R

## Installer R et RStudio

1. [Installer R sur le site officiel (CRAN)](https://cran.r-project.org/)
2. [Installer RStudio](https://posit.co/download/rstudio-desktop/)

## Les objets et les vecteurs

1. Dans le cours suivant ([lien ici](https://asardell.github.io/programmation-r/presentation.html#objets)), suivre la partie sur les objets en effectuant quelques tests et manipulation dans un script.
Le but n'est pas de tout retenir mais de comprendre la mécanique du langage.

2. Testez vous sur un quiz. Si vous souhaitez être classé pour obtenir un bonus, saisissez votre numéro étudiant. <br>
Le code du quiz vous sera transmis lors du TD.
