qfolds = 5
divi  <- rep( 1, qfolds )
division = divi
start=1
bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )
bloque

seq(1,qfolds)

N=10
rep( bloque, ceiling(N/length(bloque)))
sample( rep( bloque, ceiling(N/length(bloque))) )[1:N]