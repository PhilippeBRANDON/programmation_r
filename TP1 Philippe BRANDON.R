# 1. Créer une fonction qui retourne le salaire net avant impot pour les non cadres 
brutToNet1 <- function(brut) {
  if (!is.numeric(brut)) {
    return("ERROR : type not expected")
  }
  cotisations <- brut * 0.22 
  net <- brut - cotisations
  return(net)
}

