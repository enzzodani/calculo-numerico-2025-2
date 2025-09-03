fpoly <- function(coefs, x) {
    resultado <- 0
    
    for(i in 1:length(coefs)) {
      termo <- coefs[i]*x^(i-1)

      resultado <- resultado + termo
    }

    return(resultado)
}
