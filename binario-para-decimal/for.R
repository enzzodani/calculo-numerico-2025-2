conversor_binario_recursivo <- function(numero_binario_string) {
  digitos <- as.numeric(strsplit(numero_binario_string, "")[[1]])
  
  return(.helper_recursivo(tail(digitos, -1), digitos[1]))
}

# --- Função auxiliar recursiva ---
.helper_recursivo <- function(digitos_restantes, b_valor) {
  if (length(digitos_restantes) == 0) {
    return(b_valor)
  } else {
    a_atual <- digitos_restantes[1]
    
    b_novo <- a_atual + 2 * b_valor
    
    return(.helper_recursivo(tail(digitos_restantes, -1), b_novo))
  }
}

numero <- readline("Binario para decimal: ")
resultado <- conversor_binario_recursivo(numero)
print(paste("O decimal é:", resultado))

