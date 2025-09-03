
# Sortear o numero
n <- sample(1:50, size=1)
# Input
user <- readline("Advinhe um numero ")

# While
while (user != n) {
  if (user < n) {
    user <- readline("Tente um numero maior ")
  } else {
    user <- readline("Tente um numero menor ")
  }
}

print("Parabens, voce acertou")

