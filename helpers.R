library(ElemStatLearn)
library(igraph)

x <- zip.train[,-1]
y <- zip.train[,1]

# for zip data:
row2image <- function(line) {
  im <- t(matrix(line, 16, 16, byrow = TRUE))
  # return:
  im[, 16:1]
}

entropy <- function(ss, y){
  tab <- table(y[ss])
  tab <- tab/sum(tab)
  sum(-tab*log(tab))
}

make.clusters <- function(dat, vec, nbins, klevel=10, clustersize=100){
  
  mn <- min(vec)
  mx <- max(vec)
  
  # bin construction:
  binwidth <- (mx - mn)/nbins
  delta <- binwidth/5
  bins <- cbind( mn-delta+binwidth*(0:(nbins-1)),
                 mn+delta+binwidth*(1:nbins) )
  dat.subsets <- list()
  for(i in 1:nbins) dat.subsets[[i]] <- which( vec>bins[i,1] & vec<bins[i,2] )
  
  # hierarchical clustering within bins:
  cluster.set <- list()
  ccount <- 0
  for(i in 1:nbins){
    ss <- dat.subsets[[i]]
    if(length(ss)==0) next
    dmat <- dist(dat[ss,], method="euclidean")
    if(length(ss)==1){
      cnt <- 1
    } else {
      hc <- hclust(dmat)
      if(klevel==0) klevel <- max(1, ceiling(length(ss)/clustersize))
      ct <- cutree(hc, k=min(klevel, length(ss)))
      cnt <- max(ct)
    }
    for(j in 1:cnt){
      ccount <- ccount + 1
      tmp <- ss[ct==j]; tmp <- tmp[!is.na(tmp)]
      cluster.set[[ccount]] <- tmp
    }
  }
  
  # dedupe:
  cluster.set <- unique(cluster.set)
  
  # return:
  cluster.set
}

print.clusters <- function(cset){
          table( sapply(cset, length) ) 
}


show.entropy <- function(cluster.set, y){
  
  cs <- sapply(cluster.set, length)
  ce <- sapply(cluster.set, function(s){ entropy(s,y) } )
  
  par(mfrow=c(2,1))
  plot(sort(ce), 
       xlab="Cluster",
       ylab="Entropy",
       main=sprintf("Average entropy: %g", sum(cs*ce)/sum(cs)),
       type='h', col='blue', frame.plot=0)
  plot(ce ~ cs, 
       xlab="Cluster size",
       ylab="Cluster entropy",
       col='blue', frame.plot=0)
  par(mfrow=c(1,1))

}

mapper.graph <- function(cluster.set){
  
  ccount <- length(cluster.set)
  madj <- matrix(0, nrow=ccount, ncol=ccount)
  for(i in 1:(ccount-1))
    for(j in (i+1):ccount){
      si <- cluster.set[[i]]
      sj <- cluster.set[[j]]
      madj[i,j] <- (length(unique(si)) + length(unique(sj)) - length(unique(c(si,sj))) > 0)
      madj[j,i] <- madj[i,j]
    }
  # return:
  graph.adjacency(madj)

}

components <- function(g){

  cc <- clusters(g)
  # return:
  lapply(1:cc$no, function(i){ 
    induced.subgraph(g, which(cc$membership == i)) })
}

compsets <- function(g, cset, j){
  
  cc <- clusters(g)
  set <- as.numeric( V(g)[cc$membership==j] )
   # return:
  lapply(set, function(s) cset[[s]])
}

show.graph <- function(g, cset, v=-1, layout=layout.fruchterman.reingold){
  
  cc <- clusters(g)
  E(g)$color <- "grey"
  E(g)$width <- 1
  E(g)$arrow.mode <- 0
  E(g)$curved <- FALSE
  V(g)$label.cex <-  0.3*(1 + log(sapply(cset, length)))
  V(g)$size <- 6 * V(g)$label.cex
  V(g)$color <- "white"
  V(g)$frame.color <- cc$membership #"grey"
  V(g)$label <- ""
  V(g)$label <- sapply(V(g), function(v){ 
    tmp <- y[cset[[v]]];
    names(sort(-table(tmp)))[1]
  })
  tmp <-  log(sapply(cset, length))
  V(g)$label.color <- cc$membership #"blue"
  par(mai=c(0,0,0,0))
  if(v > 0){
    V(g)$color[v] <- "orange"
    plot(g, layout=layout)
  } else {
    plot(g, layout=layout)
  }
}

show.digits <- function(cset, v){
  
  size <- min(100, length(cset[[v]]))
  digits <- vector(length=size, mode="list")
  set <- sample(cset[[v]], size)
  digits <- x[set,]
  nrow <- ceiling(size/10)
  tmp <- lapply(1:nrow, function(i){ 
    do.call("rbind", 
            lapply((10*(i-1)):min(10*i, size), function(j){ 
              row2image(digits[j,]) }) )
  })
  if(10*nrow > size) for(i in 1:(10*nrow - size)) 
    tmp[[nrow]] <- rbind(tmp[[nrow]], matrix(0,nrow=16, ncol=16))
  im <- do.call("cbind", tmp)  
  par(mai=c(0,0,0,0))
  image( im,
         col=gray((256:0)/256), 
         zlim=c(0,1), xlab="", ylab="", asp=nrow/10,
         axes=FALSE )

}

print.digits <- function(cset, v){
  table( y[cset[[v]]] )
}

