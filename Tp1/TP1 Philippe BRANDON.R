# 2.1.1 Créer une fonction qui retourne le salaire net avant impot pour les non cadres 
brutToNet1 <- function(brut) {
  if (!is.numeric(brut)) {
    return("ERROR : type not expected")
  }
  cotisations <- brut * 0.22 
  net <- brut - cotisations
  return(net)
}

# 2.1.2
brutToNet2 <- function(brut, contrat) {
  if (!is.numeric(brut)) {
    return("ERROR : type not expected")
  }
  if (contrat != "cadre" && contrat != "non-cadre") {
    return("ERROR : contract unknown")
  }
  cotisations <- ifelse(contrat == "cadre", 0.25, 0.22) * brut
  impots <- (brut - cotisations)*0.075
  net <- brut - impots - cotisations
  return(net)
}

#2.1.3
brutToNet3 <- function(brut, contrat, taux = 0.075, temps = 100) {
  if (!is.numeric(brut) || !is.numeric(taux) || !is.numeric(temps)) {
    return("ERROR : type not expected")
  }
  if (contrat != "cadre" && contrat != "non-cadre") {
    return("ERROR : contract unknown")
  }
  if (taux < 0 || taux > 1 || temps < 0 || temps > 100) {
    return("ERROR : rate and time must be in range(0,100)")
  }
  cotisations <- ifelse(contrat == "cadre", 0.25, 0.22) * brut * temps / 100
  impots <- taux * (brut - cotisations)
  Netavantimpot <- brut - cotisations
  Netapresimpot <- Netavantimpot - impots
  return(list(Netavantimpot = Netavantimpot, Netapresimpot = Netapresimpot))
}