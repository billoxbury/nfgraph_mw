
server_calc <- list()
server_out <- list()


####################################################################
# graph layout selection

server_calc[["comm_coords"]] <- function(calc, sess){
  observeEvent(calc$coordchoice,
               {
                 calc[["comm_coords"]] <- switch(calc$coordchoice,
                                                 "DRL" = drl_lout,
                                                 "Fruchterman-Reingold" = fr_lout,
                                                 "LINE 1st-order" = line_lout_1,
                                                 "LINE 2nd-order" = line_lout_2,
                                                 "LINE mixed" = line_lout_12)
               })
}

####################################################################
# port selection

server_calc[["inselect"]] <- function(calc, sess){
  observeEvent(calc$port,
    {
      calc[["inselect"]] <- if(calc$port  == "Any"){ 
        NULL
      } else {
        which(in.port.fprob[, which(ports==calc$port)] > 0.0)
      } 
    })  
}

server_calc[["outselect"]] <- function(calc, sess){
  observeEvent(calc$port,
               {
                 calc[["outselect"]] <- if(calc$port  == "Any"){ 
                   NULL
                 } else {
                   which(out.port.fprob[, which(ports==calc$port)] > 0.0)
                 } 
               })  
}


########################################################################
# subset selection:

server_calc[["subset_func"]] <- function(calc, sess){
  observeEvent(list( calc$func_plot_brush ),
               {
                 res <- brushedPoints(func_coords, req(calc$func_plot_brush), "x1", "x2")
                 if(nrow(res) > 0){
                   calc[["subset_func"]] <- as.numeric(row.names(res))
                 } else {
                   calc[["subset_func"]] <- c()
                 }
               })
}

server_calc[["subset_comm"]] <- function(calc, sess){
  observeEvent(list( calc$comm_coords, 
                     calc$comm_plot_brush ),
               {
                 res <- brushedPoints(req(calc$comm_coords), req(calc$comm_plot_brush), "x1", "x2")
                 if(nrow(res) > 0){
                   calc[["subset_comm"]] <- as.numeric(row.names(res))
                 } else {
                   calc[["subset_comm"]] <- c()
                 }
               })
}

server_calc[["commselection"]] <- function(calc, sess){
  observeEvent(list( calc$subset_comm ),
               {
                   calc[["commselection"]] <- !is.null(calc$subset_comm)
               })
}
  
########################################################################
# selection for zoom graph:
  
server_calc[["subv"]] <- function(calc, sess){
  observeEvent(list( calc$commselection,
                     calc$subset_comm,
                     calc$subset_func ),
               {
                 calc[["subv"]] <- if(calc$commselection){ calc$subset_comm 
                 } else { calc$subset_func }
               })
}

server_calc[["currentgraph"]] <- function(calc, sess){
  observeEvent(list( calc$subv ),
               {
                 calc[["currentgraph"]] <- induced_subgraph(g, calc$subv)
               })
}

server_calc[["subsetdf"]] <- function(calc, sess){
  observeEvent(list( calc$currentgraph,
                     calc$zoomcoords,
                     calc$subv,
                     calc$comm_coords
                     ),
               {
                 g <- req(calc$currentgraph)
                 df <- switch(req(calc$zoomcoords),
                              "Communications layout" = req(calc$comm_coords)[req(calc$subv),],
                              "Functional layout" = func_coords[calc$subv,],
                              "Auto adjust" = data.frame(layout_nicely(g))
                 )
                 row.names(df) <- calc$subv
                 names(df) <- c("x1","x2")
              
                 calc[["subsetdf"]] <- df
               })
}


####################################################################
# plotting parameters:

server_calc[["col_func"]] <- function(calc, sess){ 
    observeEvent( list( calc$port, 
                        calc$subset_comm, 
                        calc$inselect, 
                        calc$outselect ), {
      
      n <- length(bbox)
      out <-  rep("lightgrey", n)
      
      if(req(calc$port) == "Any" & is.null(calc$subset_comm)){
        out <- rep("blue", n)
      } else if(calc$port  == "Any"){
        out[calc$subset_comm] <- "blue"
      } else if(is.null(calc$subset_comm)){
        out[req(calc$inselect)] <- "red"
        out[req(calc$outselect)] <- "limegreen"
        out[intersect(calc$inselect, calc$outselect)] <- "blue"
      } else {
        out[intersect(calc$inselect, calc$subset_comm)] <- "red"
        out[intersect(calc$outselect, calc$subset_comm)] <- "limegreen"
        both <- intersect(calc$inselect, calc$outselect)
        out[intersect(both, calc$subset_comm)] <- "blue"
      }
      
      calc[["col_func"]] <- out
    })
}

server_calc[["col_comm"]] <- function(calc, sess){ 
  observeEvent( list( calc$port, 
                      calc$subset_func, 
                      calc$inselect, 
                      calc$outselect ), {
                        
                        n <- length(bbox)
                        out <-  rep("lightgrey", n)
                        
                        if(req(calc$port) == "Any" & is.null(calc$subset_func)){
                          out <- rep("blue", n)
                        } else if(calc$port  == "Any"){
                          out[calc$subset_func] <- "blue"
                        } else if(is.null(calc$subset_func)){
                          out[req(calc$inselect)] <- "red"
                          out[req(calc$outselect)] <- "limegreen"
                          out[intersect(calc$inselect, calc$outselect)] <- "blue"
                        } else {
                          out[intersect(calc$inselect, calc$subset_func)] <- "red"
                          out[intersect(calc$outselect, calc$subset_func)] <- "limegreen"
                          both <- intersect(calc$inselect, calc$outselect)
                          out[intersect(both, calc$subset_func)] <- "blue"
                        }
                        
                        calc[["col_comm"]] <- out
                      })
}

server_calc[["cex_func"]] <- function(calc, sess){ 
  observeEvent( list( calc$port, 
                      calc$subset_comm, 
                      calc$inselect, 
                      calc$outselect ), {
                        
                        n <- length(bbox)
                        mn <- min(log(nbyte,2))
                        mx <- max(log(nbyte,2))
                        out <-  0.01 + (mx - log(nbyte,2))/(mx - mn)/2
                        
                        if(req(calc$port)  == "Any" & !is.null(req(calc$subset_comm))){
                          out[calc$subset_comm] <- cex_factor*out[calc$subset_comm]
                        } else if(is.null(calc$subset_comm)){
                          out[calc$inselect] <- 1.5 * out[calc$inselect]
                          out[calc$outselect] <- 1.5 * out[calc$outselect]
                          out
                        } else{
                          out[calc$inselect] <- 1.5 * out[calc$inselect]
                          out[calc$outselect] <- 1.5 * out[calc$outselect]
                          out[calc$subset_comm] <- cex_factor*out[calc$subset_comm]
                        }
                        
                        calc[["cex_func"]] <- out
                      })
}
                        
server_calc[["cex_comm"]] <- function(calc, sess){ 
  observeEvent( list( calc$subset_func ), {
                        
                        n <- length(bbox)
                        out <-  0.1*log(1 + deg)
                        if(!is.null(calc$subset_func)){
                          out[calc$subset_func] <- cex_factor*out[calc$subset_func]
                        } 
                        
                        calc[["cex_comm"]] <- out
                      })
}

server_calc[["pch_func"]] <- function(calc, sess){ 
  observeEvent( list( calc$port, 
                      calc$subset_comm, 
                      calc$inselect, 
                      calc$outselect ), {
                        
                        n <- length(bbox)
                        out <-  rep(22, n)
                        if(req(calc$port)  != "Any"){
                          out[calc$inselect] <- 15
                          out[calc$outselect] <- 15
                        }
                        if(!is.null(calc$subset_comm)){
                          out[calc$subset_comm] <- 15
                        }
                        calc[["pch_func"]] <- out
                      })
}

server_calc[["pch_comm"]] <- function(calc, sess){ 
  observeEvent( list( calc$subset_func ), {
                        
                        n <- length(bbox)
                        out <-  rep(1, n)
                        if(!is.null(calc$subset_func)){
                          out[calc$subset_func] <- 19
                        }
                        calc[["pch_comm"]] <- out
                      })
}


####################################################################
# hover-over indexes for box profiles

server_calc[["idx"]] <- function(calc, sess){ 
  observeEvent( list( calc$subsetdf,
                      calc$plot_click,
                      calc$plot_hover ), {
    
    res_click <- nearPoints(req(calc$subsetdf), req(calc$plot_click), 
                            "x1", "x2", threshold=10, maxpoints=1)
    res_hover <- nearPoints(req(calc$subsetdf), req(calc$plot_hover), 
                            "x1", "x2", threshold=10, maxpoints=1)
    calc[["idx"]] <- if(nrow(res_click)>0){ 
      as.numeric(row.names(res_click)) 
    } else if(nrow(res_hover)>0){ 
        as.numeric(row.names(res_hover)) 
    } else NULL
  })
}

####################################################################
# port/protocol temperatures

# protocol
server_calc[["out.proto.f.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["out.proto.f.feat"]] <- out.proto.fprob^(1/calc$proto_temp)      
  }) 
}
server_calc[["out.proto.b.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["out.proto.b.feat"]] <- out.proto.bprob^(1/calc$proto_temp)      
  }) 
}
server_calc[["out.proto.s.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["out.proto.s.feat"]] <- out.proto.sprob^(1/calc$proto_temp)      
  }) 
}
server_calc[["in.proto.f.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["in.proto.f.feat"]] <- in.proto.fprob^(1/calc$proto_temp)      
  }) 
}
server_calc[["in.proto.b.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["in.proto.b.feat"]] <- in.proto.bprob^(1/calc$proto_temp)      
  }) 
}
server_calc[["in.proto.s.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$proto_temp), {
    calc[["in.proto.s.feat"]] <- in.proto.sprob^(1/calc$proto_temp)      
  }) 
}

# port
server_calc[["out.port.f.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["out.port.f.feat"]] <- out.port.fprob^(1/calc$port_temp)      
  }) 
}
server_calc[["out.port.b.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["out.port.b.feat"]] <- out.port.bprob^(1/calc$port_temp)      
  }) 
}
server_calc[["out.port.s.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["out.port.s.feat"]] <- out.port.sprob^(1/calc$port_temp)      
  }) 
}
server_calc[["in.port.f.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["in.port.f.feat"]] <- in.port.fprob^(1/calc$port_temp)      
  }) 
}
server_calc[["in.port.b.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["in.port.b.feat"]] <- in.port.bprob^(1/calc$port_temp)      
  }) 
}
server_calc[["in.port.s.feat"]] <- function(calc, sess){ 
  observeEvent( req(calc$port_temp), {
    calc[["in.port.s.feat"]] <- in.port.sprob^(1/calc$port_temp)      
  }) 
}


####################################################################
####################################################################
# main plot - functional view:

server_out[["funcplot"]] <- function(calc, sess){
  renderPlot({
    #par(mai=c(0,0,0,0))
    plot(func_coords, 
         cex = calc$cex_func, 
         col = calc$col_func, 
         pch = calc$pch_func, 
         axes = FALSE, 
         frame.plot = TRUE, 
         xlab = "", 
         ylab = "")
  })
}


####################################################################
# main plot - comms view:

server_out[["commplot"]] <- function(calc, sess){
  renderPlot({
    #par(mai=c(0,0,0,0))
    handplot(g, 
             calc$comm_coords, 
             cex=calc$cex_comm, 
             col=calc$col_comm, 
             pch=calc$pch_comm,
             prob = calc$edgefrac / 100
    )
  })
}


####################################################################
# zoom graph plot:

server_out[["subplot"]] <- function(calc, sess){
  renderPlot({
    if(is.null(calc$comm_plot_brush) & is.null(calc$func_plot_brush)) return()
    if(!calc$drawzoom) return()
    g <- calc$currentgraph
    col <- if(calc$commselection) calc$col_comm[calc$subv] else calc$col_func[calc$subv]
    handsubplot(g, calc$subsetdf, col=col)
  })
}


########################################################################
# hover-over for box profiles:

server_out[["plot_hover_proto"]] <- function(calc, sess){
  renderPlot({
    show.box.proto(calc$idx,
                 calc$in.proto.f.feat,
                 calc$in.proto.b.feat,
                 calc$in.proto.s.feat,
                 calc$out.proto.f.feat,
                 calc$out.proto.b.feat,
                 calc$out.proto.s.feat)
  })
}

server_out[["plot_hover_port"]] <- function(calc, sess){
  renderPlot({
    show.box.port(calc$idx,
                calc$in.port.f.feat,
                calc$in.port.b.feat,
                calc$in.port.s.feat,
                calc$out.port.f.feat,
                calc$out.port.b.feat,
                calc$out.port.s.feat)
  })
}

####################################################################
# debug output:

server_calc[["debug"]] <- function(calc, sess){
  observeEvent(list( calc$subset_func ), {
    calc[["verb"]] <- calc$subset_func
  })
}

server_out[["verb"]] <- function(calc, sess){
  renderPrint({ calc$verb })
}
