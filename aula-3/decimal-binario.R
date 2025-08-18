conversor_decimal_binario_recursivo <- function(numero_decimal) {
  
  if (numero_decimal == 0) {
    return("0")
  }
  
  return(.helper_decimal_recursivo(numero_decimal))
}


.helper_decimal_recursivo <- function(quociente) {
  
  if (quociente == 0) {
    return("")
  } else {
    novo_quociente <- floor(quociente / 2)
    
    resto <- quociente %% 2
    
    resultado_da_recursao <- .helper_decimal_recursivo(novo_quociente)
    
    return(paste0(resultado_da_recursao, resto))
  }
}

# --- Exemplos de Uso ---
print(paste("Decimal 2021 em binário é:", conversor_decimal_binario_recursivo(2021)))

