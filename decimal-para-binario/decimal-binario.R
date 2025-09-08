# --- Ferramentas para Conversão de Decimal para Binário ---

# --- 1. Funções para a Parte Inteira ---

# Função auxiliar recursiva para a parte inteira (Divisões sucessivas)
# @param quociente O número a ser processado no passo atual da recursão.
.helper_decimal_recursivo <- function(quociente) {
  # Caso Base: A recursão para quando o quociente chega a zero.
  if (quociente == 0) {
    return("")
  } else {
    # Passo Recursivo:
    novo_quociente <- floor(quociente / 2)
    resto <- quociente %% 2
    
    # A recursão é chamada primeiro para construir o número na ordem correta (da esquerda para a direita).
    resultado_da_recursao <- .helper_decimal_recursivo(novo_quociente)
    
    return(paste0(resultado_da_recursao, resto))
  }
}

# Função principal para converter a parte inteira de um decimal para binário.
# @param numero_decimal O número inteiro a ser convertido.
conversor_decimal_binario_recursivo <- function(numero_decimal) {
  # O caso para o número 0 é tratado aqui, pois a recursão resulta em string vazia.
  if (numero_decimal == 0) {
    return("0")
  }
  return(.helper_decimal_recursivo(numero_decimal))
}


# --- 2. Funções para a Parte Fracionária ---

# Função auxiliar recursiva para a parte fracionária (Multiplicações sucessivas)
# @param numero O valor fracionário a ser processado.
# @param limite Contador para evitar dízimas infinitas e limitar a precisão.
.helper_fracao_recursivo <- function(numero, limite) {
  # Caso Base: Para se o número zerar ou se o limite de precisão for atingido.
  if (numero == 0 || limite == 0) {
    return("")
  }

  # Passo Recursivo:
  novo_numero <- numero * 2
  digito <- floor(novo_numero) # O dígito binário é a parte inteira da multiplicação.
  resto <- novo_numero - digito # O resto continua para o próximo passo.
  
  return(paste0(digito, .helper_fracao_recursivo(resto, limite - 1)))
}

# Função principal para converter a parte fracionária.
# @param numero_fracao O número fracionário (entre 0 e 1) a ser convertido.
# @param limite A quantidade máxima de casas decimais no binário.
conversor_decimal_binario_fracao_recursivo <- function(numero_fracao, limite = 10) {
  return(.helper_fracao_recursivo(numero_fracao, limite))
}


# --- 3. Função Principal de Orquestração ---

# Combina as conversões da parte inteira e fracionária.
# @param numero O número decimal completo a ser convertido.
conversor_completo_decimal_binario <- function(numero) {
  if (numero == 0) {
    return("0")
  }

  # Separa o número em suas duas componentes.
  parte_inteira <- floor(numero)
  parte_fracionada <- numero - parte_inteira

  # Chama as funções especializadas para cada parte.
  binario_inteiro <- conversor_decimal_binario_recursivo(parte_inteira)
  binario_fracao <- conversor_decimal_binario_fracao_recursivo(parte_fracionada)

  # Junta os resultados.
  if (parte_fracionada == 0) {
    return(binario_inteiro)
  } else {
    return(paste0(binario_inteiro, ".", binario_fracao))
  }
}


# --- Exemplos de Uso (para teste) ---
print(paste("Decimal 272.937 em binário é:", conversor_completo_decimal_binario(272.937)))
