.decimal_floor <- function(x, decimals=10)
{
    n <- length(x)
    check.dig <- 1/(10^(0:decimals))
    floor.x <- rep.int(NA, n)
    neg <- x < 0
    x <- abs(x)
    for(i in 1:n) {
        digits <- sum(x[i] < decimals)
        if(neg[i])
            floor.x[i] <- -.decimal_ceiling(x[i])
        else
            floor.x[i] <- floor(x[i]*(10^digits)) / (10^digits)
    }
    return(floor.x)
}
.decimal_ceiling <- function(x, decimals=10)
{
    n <- length(x)
    check.dig <- 1/(10^(0:decimals))
    ceiling.x <- rep.int(NA, n)
    neg <- x < 0
    x <- abs(x)
    for(i in 1:n) {
        digits <- sum(x[i] < decimals)
        if(neg[i])
            ceiling.x[i] <- -.decimal_floor(x[i])
        else
            ceiling.x[i] <- ceiling(x[i]*(10^digits)) / (10^digits)
    }
    return(ceiling.x)
}
