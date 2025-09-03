secante <- function(funcao, x0, x1, erro1, erro2, i=1) {
    if (abs(funcao(x1)) < erro1 || abs(x0 - x1) < erro2) {
        return(x1)
    }

    x2 = (x0*funcao(x1) - x1*funcao(x0))/(funcao(x1) - funcao(x0))
    i = i + 1

    cat("x",i, " = ", x2, "\n")

    return(secante(funcao, x1, x2, erro1, erro2,i))

}

funcao_teste <- function(x) {
    return(x^(2) + x - 6)
}

secante(funcao_teste,1.5,1.7,0.0001,0.0002)
