n_str <- readline("Coloque um numero n ")
n <- as.integer(n_str)

for (i in 1:n) {
  if (i %% 3 == 0) {
    print(i)
  }
}
