# library(matlib) 

gan_kaggle = -251.73721 * 10^6
cant_reg_kaggle = 238986

costo_pos = 48750
costo_neg = -1250

A <- matrix(c(1,costo_pos,1,costo_neg),2,2)
b <- c(cant_reg_kaggle,gan_kaggle)

# showEqn(A,b)
sol <- solve(A, b)

cant_pos = sol[1]
cant_neg = sol[2]

cant_pos
cant_neg
cant_pos/(cant_reg_kaggle)