dir <- "/home/marinho/Documents/calculon/aula-3/decimal-binario.R"
source(dir)

# --- Conversão da Parte Fracionada ---
# Função principal para a parte fracionada.
# O parâmetro 'limite' controla a precisão.
conversor_decimal_binario_fracao_recursivo <- function(numero_fracao, limite = 10) {
  return(.helper_fracao_recursivo(numero_fracao, limite))
}

.helper_fracao_recursivo <- function(numero, limite) {
  if (numero == 0 || limite == 0) {
    return("")
  }

  novo_numero <- numero * 2
  digito <- floor(novo_numero)
  resto <- novo_numero - digito
  
  return(paste0(digito, .helper_fracao_recursivo(resto, limite - 1)))
}

# --- Função Principal de Conversão Completa ---
# Esta função combina a parte inteira e a parte fracionada.
conversor_completo_decimal_binario <- function(numero) {
  if (numero == 0) {
    return("0")
  }

  parte_inteira <- floor(numero)
  parte_fracionada <- numero - parte_inteira

  binario_inteiro <- conversor_decimal_binario_recursivo(parte_inteira)
  binario_fracao <- conversor_decimal_binario_fracao_recursivo(parte_fracionada)

  if (parte_fracionada == 0) {
    return(binario_inteiro)
  } else {
    return(paste0(binario_inteiro, ".", binario_fracao))
  }
}

# --- Exemplos de Uso ---
print(paste("Decimal 20.25 em binário é:", conversor_completo_decimal_binario(20.25)))
