setClass("rmfit",
    slots = c(
        fit.indices = "vector"
    )
)

setMethod("show","rmfit",
    function(object) {
        print(round(object@fit.indices,2))
    }
)

rmfit <- function(model, person.par=NULL)
{
    # 'model': Un modello di classe dRm/Rm/eRm
    fit <- list(item=NULL,person=NULL)
    # Items
    k <- length(model$betapar)
    MSE2 <- sum(model$se.beta^2)/k
    SD2 <- sum((model$betapar-mean(model$betapar))^2)/k
    SA2 <- SD2 - MSE2
    G <- sqrt(SA2/MSE2)
    H <- (4*G+1)/3
    R <- SA2/SD2
    fit$item <- c(SA2=SA2,SD2=SD2,RMSE=sqrt(MSE2),G=G,H=H,R=R)
    rm(k,MSE2,SD2,SA2,G,H,R)
    # Persons
    if(is.null(person.par))
        person.par <- person.parameter(model)$thetapar[[1]]
    k <- length(person.par)
    MSE2 <- sum(person.par^2)/k
    SD2 <- sum((person.par-mean(person.par))^2)/k
    SA2 <- SD2 - MSE2
    G <- sqrt(SA2/MSE2)
    H <- (4*G+1)/3
    R <- SA2/SD2
    new("rmfit", fit.indices=c(SA2=SA2,SD2=SD2,RMSE=sqrt(MSE2),G=G,H=H,R=R))
}
