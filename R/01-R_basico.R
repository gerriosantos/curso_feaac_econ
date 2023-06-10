

# Operacoes basicas R ----------------------------------------------------------


## Operacoes de diretorios ----
#
getwd()
setwd()

dir('data-raw')

dir.create()
file.create()



basename()
dirname()


## Operacoes principais ----

1+1

1 > 2

4 / 2


2 ^ 2

2 ** 2

1:50

50:100


## Objetos ----

h_obj <- 12

obj <- 1

obj + 10

obj_2 <- obj + 10

obj <- 10

Obj <- 10




# Estrutura de Dados -----------------------------------------------------------

# Vetores ----

help(c())

?c()

obj_vetor <- c(1, 2, 3, 4, 5)

# Alt + hifen (<-)

obj_vetor2 <- 1:5

# 3 tipos

# Numericos
obj_vetor <- c(1.3, 2.4, 3.5, 4.0, 5.0)

obj_int <- c(1L, 2L)

class(obj_vetor)

# characteres

obj_char <- c("a", "gerrio", 'bruna')

class(obj_char)


# logico

obj_log <- c(F, T, F, T)
class(obj_log)


?seq()
help(seq)

obj_seq <- seq(1, 100, 2)

?rep()

1:3
c(1,2,3)

rep(1:3, times = 2)

v_rep <- rep(x = c(1,2,3), each = 2)

unique(c(1,1,2,3,4,5,5,5))


m <- matrix(data = 1:12, nrow = 3, ncol = 4)





# heterogeneo

vetor_1 <- c(1, T)

character > numeric > logical

# Listas ----



lista <- list(v_1 = c(1L, 2L), v_2 = c('a', 'b', 'c'), list(c(F, T)),
              df = data.frame(v1 = c(1,2,3), v2 = c('q', 'p', 'd')))

typeof(lista[[1]])

lista[1]

lista[[1]][[3]]

lista$v_1

lista[[3]]

lista$v_2

lista[[4]]

lista$df


# Data Frames ----


obj <- 1

obj <- c(1, 2, 3)


obj <- list(v1 = c(12, 13), v2 = c('q', 'p'))


df <- data.frame(v1 = c(12, 13), v2 = c('q', 'p'))


# --












