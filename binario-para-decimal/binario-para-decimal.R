# Função principal que gerencia a conversão de um binário (inteiro ou fracionário) para decimal.
# @param numero_binario_string A string com o número binário. Ex: "1101.101".
conversor_binario_recursivo <- function(numero_binario_string) {
  
  # --- 1. Separa a parte inteira da parte fracionária ---
  # O "[,.]" lida tanto com vírgula quanto com ponto como separador.
  partes <- strsplit(numero_binario_string, "[,.]")[[1]]
  parte_inteira_str <- partes[1]
  
  # --- 2. Processa a Parte Inteira ---
  digitos_int <- as.numeric(strsplit(parte_inteira_str, "")[[1]])
  resultado_int <- .helper_recursivo(tail(digitos_int, -1), digitos_int[1])
  
  # --- 3. Processa a Parte Fracionária (se existir) ---
  resultado_frac <- 0
  if (length(partes) > 1) {
    parte_frac_str <- partes[2]
    digitos_frac <- as.numeric(strsplit(parte_frac_str, "")[[1]])
    
    # Loop para calcular o valor fracionário.
    # A lógica é: d_1*2^{-1} + d_2*2^{-2} + ...
    for (i in 1:length(digitos_frac)) {
      resultado_frac <- resultado_frac + digitos_frac[i] * 2^(-i)
    }
  }
  
  # --- 4. Soma as duas partes para o resultado final ---
  return(resultado_int + resultado_frac)
}


# --- Função auxiliar recursiva ---
# Processa a parte inteira do número binário.
.helper_recursivo <- function(digitos_restantes, b_valor) {
  # Caso Base: Se não há mais dígitos, o valor acumulado é o resultado.
  if (length(digitos_restantes) == 0) {
    return(b_valor)
  } 
  # Passo Recursivo:
  else {
    a_atual <- digitos_restantes[1]
    # Aplica a fórmula de Horner: novo_valor = proximo_digito + 2 * valor_anterior
    b_novo <- a_atual + 2 * b_valor
    # Chama a função novamente com o resto dos dígitos.
    return(.helper_recursivo(tail(digitos_restantes, -1), b_novo))
  }
}


# --- Bloco de Execução ---
numero <- "1001.1001"
resultado <- conversor_binario_recursivo(numero)
print(paste("O resultado para", numero, "eh: ", resultado))
