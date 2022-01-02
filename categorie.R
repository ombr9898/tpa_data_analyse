library(ggplot2)
library(tidyverse)

-----------catalogue_catégorie

catalogue$categorie=catalogue$nom
catalogue$categorie=case_when(
  startsWith(catalogue$nom, 'S80') ~ 'Routiere',
  startsWith(catalogue$nom, 'Touran') ~ 'Monospace',
  startsWith(catalogue$nom, 'Polo') ~ 'Citadine',
  startsWith(catalogue$nom, 'New') ~ 'Citadine',
  startsWith(catalogue$nom, 'Golf') ~ 'Compacte',
  startsWith(catalogue$nom, 'Superb') ~ 'Familiale',
  startsWith(catalogue$nom, 'Toledo') ~ 'Familiale',
  startsWith(catalogue$nom, '9.3 ') ~ 'Familiale',
 startsWith(catalogue$nom, 'Vel Satis') ~ 'Routiere',
 startsWith(catalogue$nom, 'Megane') ~ 'Compacte',
 startsWith(catalogue$nom, 'Laguna') ~ 'Familiale',
 startsWith(catalogue$nom, 'Espace') ~ 'Monospace',
 startsWith(catalogue$nom, '1007') ~ 'Minispace',
 startsWith(catalogue$nom, 'Primera') ~ 'Familiale',
 startsWith(catalogue$nom, 'Maxima') ~ 'Routiere',
 startsWith(catalogue$nom, 'Almera') ~ 'Compacte',
 startsWith(catalogue$nom, 'Copper') ~ 'Citadine',
 startsWith(catalogue$nom, 'S500') ~ 'Limousine',
 startsWith(catalogue$nom, 'A200') ~ 'Citadine',
 startsWith(catalogue$nom, 'Ypsilon') ~ 'Citadine',
 startsWith(catalogue$nom, 'Picanto') ~ 'Citadine',
 startsWith(catalogue$nom, 'X-Type') ~ 'Berline',
 startsWith(catalogue$nom, 'Matrix') ~ 'SUV',
 startsWith(catalogue$nom, 'FR-V') ~ 'Monospace',
 startsWith(catalogue$nom, 'Mondeo') ~ 'Familiale',
 startsWith(catalogue$nom, 'Croma') ~ 'Familiale',
 startsWith(catalogue$nom, 'Cuore') ~ 'Polyvalente',
 startsWith(catalogue$nom, 'Logan') ~ 'Compacte',
 startsWith(catalogue$nom, 'M5') ~ 'Sport',
 startsWith(catalogue$nom, '120i') ~ 'Compacte',
 startsWith(catalogue$nom, 'A3') ~ 'Compacte',
 startsWith(catalogue$nom, 'A2') ~ 'Citadine',
 
)

-------------------immatriculation_catégorie

immatriculation$categorie=immatriculation$nom
immatriculation$categorie=case_when(
  startsWith(immatriculation$nom, 'S80') ~ 'Routiere',
  startsWith(immatriculation$nom, 'Touran') ~ 'Monospace',
  startsWith(immatriculation$nom, 'Polo') ~ 'Citadine',
  startsWith(immatriculation$nom, 'New') ~ 'Citadine',
  startsWith(immatriculation$nom, 'Golf') ~ 'Compacte',
  startsWith(immatriculation$nom, 'Superb') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Toledo') ~ 'Familiale',
  startsWith(immatriculation$nom, '9.3 ') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Vel Satis') ~ 'Routiere',
  startsWith(immatriculation$nom, 'Megane') ~ 'Compacte',
  startsWith(immatriculation$nom, 'Laguna') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Espace') ~ 'Monospace',
  startsWith(immatriculation$nom, '1007') ~ 'Minispace',
  startsWith(immatriculation$nom, 'Primera') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Maxima') ~ 'Routiere',
  startsWith(immatriculation$nom, 'Almera') ~ 'Compacte',
  startsWith(immatriculation$nom, 'Copper') ~ 'Citadine',
  startsWith(immatriculation$nom, 'S500') ~ 'Limousine',
  startsWith(immatriculation$nom, 'A200') ~ 'Citadine',
  startsWith(immatriculation$nom, 'Ypsilon') ~ 'Citadine',
  startsWith(immatriculation$nom, 'Picanto') ~ 'Citadine',
  startsWith(immatriculation$nom, 'X-Type') ~ 'Berline',
  startsWith(immatriculation$nom, 'Matrix') ~ 'SUV',
  startsWith(immatriculation$nom, 'FR-V') ~ 'Monospace',
  startsWith(immatriculation$nom, 'Mondeo') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Croma') ~ 'Familiale',
  startsWith(immatriculation$nom, 'Cuore') ~ 'Polyvalente',
  startsWith(immatriculation$nom, 'Logan') ~ 'Compacte',
  startsWith(immatriculation$nom, 'M5') ~ 'Sport',
  startsWith(immatriculation$nom, '120i') ~ 'Compacte',
  startsWith(immatriculation$nom, 'A3') ~ 'Compacte',
  startsWith(immatriculation$nom, 'A2') ~ 'Citadine',
  
)

