## Band-Aid – Guide de l’utilisateur

Bienvenue dans Band-Aid! Ce guide explique comment utiliser chaque fonctionnalité de l’application.

### Table des matières
1. [Téléverser vos données](#téléverser-vos-données)
2. [Appliquer des filtres](#appliquer-des-filtres)
3. [Afficher vos données dans un tableau](#afficher-vos-données-dans-un-tableau)
4. [Créer une carte](#créer-une-carte)

---

### Téléverser vos données

La section **Téléversement (Upload)** est votre point de départ. C’est ici que vous chargez vos données GameBird dans l’application.

#### Comment téléverser :
1. Cliquez sur l’onglet **Upload** au démarrage de l’application
2. Cliquez sur **Browse**
3. Sélectionnez votre fichier CSV GameBird sur votre ordinateur
4. L’application lit automatiquement votre fichier et le prépare pour l’analyse.

#### Fusion automatique des tables de référence (lookup) :
- L’application fusionne automatiquement les fichiers de référence situés dans le dossier Look Ups
- Ceux-ci peuvent inclure des tables de codes d’espèces, d’âge ou d’autres données de référence
- Vous n’avez rien à faire—cela se fait automatiquement!
- **Le téléversement du fichier principal + la fusion des tables de référence prennent environ un long 10-15 minutes**. Si vous utilisez toujours le même sous‑ensemble (p. ex., données régionales liées à un permis), **après le premier téléversement, créez ce sous‑ensemble et téléchargez‑le**, puis n’utilisez par la suite que ce sous‑ensemble pour accélérer les opérations.
- Une fois le téléversement terminé, vos données sont traitées et les filtres deviennent disponibles.

### Appliquer des filtres

La section **Filtres (Filters)** vous permet d’affiner vos données en sélectionnant des enregistrements précis.

#### Filtres disponibles :
L’application génère automatiquement des filtres selon les colonnes de vos données. Exemples courants :
- **Plage de dates**
- **Espèces**
- **Colonnes numériques** (min/max)
- **Colonnes texte** (recherche ou sélection)

#### Comment utiliser les filtres :
1. Cliquez sur l’onglet **Filters**
2. Utilisez les contrôles par colonne
3. **Listes déroulantes** : ouvrez et sélectionnez les valeurs
4. **Valeurs umériques** : saisissez min/max ou utilisez les curseurs
5. **Dates** : choisissez une plage
6. Cliquez sur **Apply**
7. Le tableau se met à jour avec les enregistrements correspondants

#### Conseils sur les filtres :
- Plusieurs filtres se combinent avec une logique **ET (AND)**
- Le nombre d’enregistrements correspondants s’affiche en bas
- Réinitialisez un filtre pour revenir à l’ensemble

---

### Afficher vos données dans un tableau

L’onglet **Table** affiche vos données filtrées dans un format structuré.

#### Fonctionnalités :
- Défilement horizontal pour voir toutes les colonnes
- Tri en cliquant sur les en‑têtes de colonnes
- Zone de recherche pour trouver des enregistrements

#### Fusion avec un fichier de stations (optionnel)
1. Déroulez **Add Station Names**
2. Téléversez un CSV/Excel contenant les informations de station
3. Une fois les champs latitude/longitude choisis, la fusion s’exécute automatiquement
4. Le tableau est enrichi avec les nouvelles informations

#### Télécharger vos données
- Utilisez les boutons **Download** sous le tableau (CSV/XLSX)

#### Navigation dans le tableau
- Contrôles de pagination
- Choix du nombre de lignes par page
- Boutons d’export en haut à droite

---

### Créer une carte

L’onglet **Map** permet de visualiser vos observations sur une carte.

#### Ce que vous verrez :
- **Marqueurs d’occurrence (Encounter)**
- **Marqueurs de station** (symboles noirs plus grands)
- Une légende

#### Sélecteur d’espèces
- Utilisez le menu à cases à cocher

#### Sélecteur de station
- Utilisez le menu à cases à cocher
- **No Station** affiche les observations sans station associée

#### Interactions
- **Zoom** : molette/pincement
- **Panoramique** : cliquer‑glisser

#### Mode d’affichage des stations
- **Centroïde** (point central des observations d’une station)
- **Plus récent** (dernière observation)

#### Exporter votre carte
- Cliquez sur **Download Map** (JPEG/PNG)
