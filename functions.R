
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

handplot <- function(g, lout, prob=0.01, 
                     cex=V(g)$size, col="blue", pch=1){
  
  plot(lout, type='n', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
  esample <- sample(E(g), prob*ecount(g), prob=E(g)$weight)
  for(e in esample){
    v <- ends(g,e, names=FALSE)
    segments(lout[v[1],1], lout[v[1],2],  lout[v[2],1], lout[v[2],2], col='grey')
  }
  points(lout, pch=pch, cex=cex, col=col)
}


handsubplot <- function(g, lout, col="grey"){
  
  #palette(heat.colors(40))
  transparency <- 120
  par(mai=c(0,0,0,0))
  plot(lout, type='n', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
  for(e in E(g)){
    v <- ends(g,e, names=FALSE)
    Arrows(lout[v[1],1], lout[v[1],2],  lout[v[2],1], lout[v[2],2], 
           col=addTrans("darkgrey", transparency))
  }
  points(lout, pch=15, cex=9*V(g)$size, col=addTrans(col, transparency))
  points(lout, pch=22, cex=9.3*V(g)$size, col=addTrans("darkblue", transparency))
  xtext <- min(lout[,1]) + 0.15*(max(lout[,1]) - min(lout[,1]))
  ytext <- min(lout[,2]) + 0.01*(max(lout[,2]) - min(lout[,2]))
  text(xtext, ytext, labels=sprintf("%d vertices, %d edges", vcount(g), ecount(g)), 
       col='darkblue')
}


########################################################################
# profile plots:

# protocol
show.box.proto <- function(idx,
                           in.proto.f.feat,
                           in.proto.b.feat,
                           in.proto.s.feat,
                           out.proto.f.feat,
                           out.proto.b.feat,
                           out.proto.s.feat){
  
  if(is.null(idx)) return()
  b <- as.character( bbox[idx] )
  tmp <- data.frame( rbind(
    in.proto.f.feat[idx,], out.proto.f.feat[idx,],
    in.proto.b.feat[idx,], out.proto.b.feat[idx,],
    in.proto.s.feat[idx,], out.proto.s.feat[idx,]
  ) )
  names(tmp) <- c("ICMP","UDP","TCP","41")
  row.names(tmp) <- c("flows", "", "bytes", " ", "time", "  ")
  col <- matrix(rep( c(
    c("red4","red","pink","lavender"),
    c("springgreen4","limegreen","lawngreen","yellow")
  ),3), nrow=6, ncol=4, byrow=TRUE)
  mos <- mosaic(as.matrix(tmp), 
                gp = gpar(fill=col), 
                labeling = TRUE, 
                direction="v",
                las=2, cex.axis=1.5, 
                main=sprintf("Computer %s", b))
}

# port

show.box.port <- function(idx,
                          in.port.f.feat,
                          in.port.b.feat,
                          in.port.s.feat,
                          out.port.f.feat,
                          out.port.b.feat,
                          out.port.s.feat){
  
  if(is.null(idx)) return()
  b <- as.character( bbox[idx] )
  mx <- max( which(in.port.f.feat[idx,] > 0 | out.port.f.feat[idx,] 
                   | in.port.b.feat[idx,] > 0 | out.port.b.feat[idx,]
                   | in.port.s.feat[idx,] > 0 | out.port.s.feat[idx,]) )
  if(mx < length(ports)) mx <- mx + 1
  index <- 1:mx
  outdex <- index+0.5
  threshold <- 0.05
  lab.in <- rep("", mx)
  lab.out <- rep("", mx)
  cond.in <- ( in.port.f.feat[idx,1:mx] > threshold  )
  cond.out <- ( out.port.f.feat[idx,1:mx] > threshold )
  lab.in[cond.in] <- ports[1:mx][cond.in]
  lab.out[cond.out] <- ports[1:mx][cond.out]
  lwd <- 5
  textangle <- -90
  par(mfrow=c(3,1), mai=c(0,0,0,0))
  plot(index, in.port.f.feat[idx,1:mx],
       main="",
       xlab="",
       ylab = "",
       type='h', 
       ylim=c(0,1), 
       lwd=lwd, col='red', 
       yaxt='n',
       xaxt='n')
  in.txtht <- (in.port.f.feat[idx,1:mx] + 1)/2
  out.txtht <- (out.port.f.feat[idx,1:mx] + 1)/2
  text(index, in.txtht, labels=lab.in, cex=1.5, col='black', srt=textangle)
  points(out.port.f.feat[idx,1:mx] ~ outdex, 
         main=sprintf("Computer %s", b),
         type='h', 
         lwd=lwd, col='green')
  text(outdex, out.txtht, labels=lab.out, cex=1.5, col='black', srt=textangle)
  legend("topright", bty='n', cex=1.5, legend="Flows", text.col="grey")
  plot(index, in.port.b.feat[idx,1:mx],
       main="",
       xlab="",
       ylab = "",
       type='h', 
       ylim=c(0,1), 
       lwd=lwd, col='red', 
       yaxt='n',
       xaxt='n')
  legend("topright", bty='n', cex=1.5, legend="Bytes", text.col="grey")
  points(out.port.b.feat[idx,1:mx] ~ outdex, 
         main=sprintf("Computer %s", b),
         type='h', 
         lwd=lwd, col='green')
  plot(index, in.port.s.feat[idx,1:mx],
       main="",
       xlab="",
       ylab = "",
       type='h', 
       ylim=c(0,1), 
       lwd=lwd, col='red', 
       yaxt='n',
       xaxt='n')
  lab.in <- rep("", mx)
  lab.out <- rep("", mx)
  cond.in <- ( in.port.s.feat[idx,1:mx] > threshold  )
  cond.out <- ( out.port.s.feat[idx,1:mx] > threshold )
  lab.in[cond.in] <- ports[1:mx][cond.in]
  lab.out[cond.out] <- ports[1:mx][cond.out]
  in.txtht <- (in.port.s.feat[idx,1:mx] + 1)/2
  out.txtht <- (out.port.s.feat[idx,1:mx] + 1)/2
  text(index, in.txtht, labels=lab.in, cex=1.5, col='black', srt=textangle)
  points(out.port.s.feat[idx,1:mx] ~ outdex, 
         main=sprintf("Computer %s", b),
         type='h', 
         lwd=lwd, col='green')
  text(outdex, out.txtht, labels=lab.out, cex=1.5, col='black', srt=textangle)
  legend("topright", bty='n', cex=1.5, legend="Seconds", text.col="grey")
}

