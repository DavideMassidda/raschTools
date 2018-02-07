setClass("RaschData",
    slots = c(
        data = "matrix",
        ability = "vector",
        difficulty = "matrix",
        Pr = "list",
        labels = "list"
    )
)

setMethod("show","RaschData",
    function(object) {
        cat("Data matrix structure:\n")
        cat("   Subjects (rows):", nrow(object@data), "\n")
        cat("   Items (columns):", ncol(object@data), "\n")
        cat("\nItem's difficulties:\n")
        print(t(object@difficulty))
        cat("\nSubject's abilities:\n")
        b <- cbind(beta=object@ability)
        rownames(b) <- object@labels$subj
        print(b)
    }
)

.random_par <- function(n,range=c(-3,3))
{
    p <- runif(n,range[1],range[2])
    p <- sort(p,decreasing=FALSE)
    p <- p-mean(p)
    return(p)
}

.random_resp_SLM <- function(p, n=1)
    rbinom(n,1,p)

.random_resp_PCM <- function(p, m)
{
    # p: vettore di probabilità associate alle k soglie di un item.
    # m: numero di soglie da superare, ovvero punteggio massimo,
    #    ovvero lunghezza di p.
    k <- 0
    resp <- 0
    repeat {
        k <- k+1
        x <- rbinom(1,1,p[k])
        if(x==1)
            resp <- resp+1
        if(x==0 | k==m)
            break
    }
    return(resp)
}

# ------------------------------------------------------------
# maxScore: punteggio massimo della scala (da 0 a maxScore)
# beta: abilita' dei soggetti
# delta: difficolta' degli item
# ------------------------------------------------------------

# Simple Logistic Model
simSLM <- function(numSubj=10,numItem=5,seed=NULL)
{
    if(!is.null(seed))
        set.seed(seed)
    nm <- list(
        subj = paste("Subject",1:numSubj),
        beta = paste("beta",1:numSubj),
        item = paste("Item",1:numItem)
    )
    beta  <- rnorm(numSubj,mean=0,sd=2)
    delta <- .random_par(numItem)
    # Normalizzazione dei parametri
    #beta <- beta - delta[1,1]
    #delta <- delta - delta[1,1]
    #delta <- delta-mean(delta)
    names(beta) <- nm$beta
    names(delta) <- nm$item
    theta <- outer(beta, delta, "-")
    # Pr di superare ogni soglia per ogni soggetto e risposte
    Pr <- exp(theta)/(1+exp(theta))
    resp <- matrix(NA,nrow=numSubj,ncol=numItem,dimnames=list(nm$subj,nm$item))
    resp <- apply(Pr, 2, .random_resp_SLM, n=numSubj)
    rownames(resp) <- nm$subj
    delta <- t(delta)
    rownames(delta) <- "delta"
    Pr <- list(delta=Pr)
    new("RaschData",data=resp,ability=beta,difficulty=delta,Pr=Pr,labels=nm)
}

# Partial Credit Model
simPCM <- function(numSubj=10,numItem=5,maxScore=3,seed=NULL)
{
    if(!is.null(seed))
        set.seed(seed)
    nm <- list(
        subj = paste("Subject",1:numSubj),
        beta = paste("beta",1:numSubj),
        delta = paste("delta",1:maxScore),
        item = paste("Item",1:numItem)
    )
    beta  <- rnorm(numSubj,mean=0,sd=2)
    delta <- replicate(numItem, .random_par(maxScore))
    # Normalizzazione dei parametri
    #beta <- beta - delta[1,1]
    #delta <- delta - delta[1,1]
    #delta <- delta-mean(delta)
    names(beta) <- nm$beta
    rownames(delta) <- nm$delta
    colnames(delta) <- nm$item
    theta <- outer(beta, delta, "-")
    # Pr di superare ogni soglia per ogni soggetto e risposte
    Pr <- vector("list",numSubj)
    resp <- matrix(NA,nrow=numSubj,ncol=numItem,dimnames=list(nm$subj,nm$item))
    names(Pr) <- nm$subj
    for(n in 1:numSubj) {
        cumTheta <- exp(apply(theta[n,,],2,cumsum))
        psy <- colSums(cumTheta)
        Pr[[n]] <- apply(cumTheta, 1, function(x) x/(1+psy))
        # Generazione risposte per ogni item
        resp[n,] <- apply(Pr[[n]], 1, .random_resp_PCM, m=maxScore)
    }
    new("RaschData",data=resp,ability=beta,difficulty=delta,Pr=Pr,labels=nm)
}
