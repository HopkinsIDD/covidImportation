

# Funcion to set monthyr as factor for sorting
set_monthyr_factor <- function(data){
    year_range <- range(as.integer(substr(data$month_yr, 5,8)), na.rm=TRUE)
    month_yr_ <- paste0(month.abb, "-", sort(rep(year_range[1]:year_range[2], 12)))
    data %>% mutate(month_yr = factor(month_yr, levels=month_yr_, ordered = TRUE))
}


## m=matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
## this function makes a graphically appealing heatmap (no dendrogram) using ggplot
## whilst it contains fewer options than gplots::heatmap.2 I prefer its style and flexibility

ggheat <- function(m, 
                   zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), 
                   title='RR Importation', 
                   x.angle=45, x.size=8, y.size=8, leg.title.size=10, leg.text.size=8,
                   aspect_ratio=3, axis_margin_y=8,
                   labCol=T, labRow=T, border=F, t.skip=6, leg.key.size=10, na.value="grey60") {
    
    
    # ## Check if length of zCuts is even (needs to be)
    # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
    
    ## the function can be be viewed as a two step process
    ## 1. using the rehape package and other funcs the data is clustered, scaled, and reshaped
    ## using simple options or by a user supplied function
    ## 2. with the now resahped data the plot, the chosen labels and plot style are built
    
    require(ggplot2)
    require(RColorBrewer)
    
    ## you can either scale by row or column not both! 
    ## if you wish to scale by both or use a differen scale method then simply supply a scale
    ## function instead NB scale is a base funct
    
    ## this is just reshaping into a ggplot format matrix and making a ggplot layer
    rows <- dim(m)[1]
    cols <- dim(m)[2]
    melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), reshape::melt(m))
    g <- ggplot(data=melt.m)
    
    ## add the heat tiles with or without a white border for clarity
    myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues")))
    #myPallette <- rev(c(rev(brewer.pal((length(zCuts)/2-1), "YlOrRd")), "white", brewer.pal((length(zCuts)/2-1), "Blues")))
    cut.intervals <- sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1]))
    
    if(border==TRUE) 
        g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, fill=cut(value, zCuts, include.lowest=T)), colour='white') +
        scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
                          guide = guide_legend(reverse = TRUE))
    if(border==FALSE)
        g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, fill=cut(value, zCuts, include.lowest=T))) +
        scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
                          guide = guide_legend(reverse = TRUE))
    
    ## add axis labels either supplied or from the colnames rownames of the matrix
    if(labCol==T) 
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames(m))
    if(labCol==F) 
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
    if(labCol=='months') 
        g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames(m)[seq(1,cols,t.skip)])
    #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
    
    if(labRow==T) 
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rownames(m))	
    if(labRow==F) 
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
    
    ## get rid of grey panel background and gridlines and tick marks
    g2 <- g2 + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background=element_blank(),
                     axis.ticks = element_blank())
    
    ## Move axis text closer to the plot
    g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
                     axis.text.x = element_text(margin = margin(t = -20), size=x.size),
                     legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
    ## Put the Axis text at an angle for X and size
    g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
    
    # Reduce the space between the legend and the panel & Legend size
    g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
                     legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
                     plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
                     plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
        coord_fixed(ratio=aspect_ratio)  
    
    return(g2)
}





## m=matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
## this function makes a graphically appealing heatmap (no dendrogram) using ggplot
## whilst it contains fewer options than gplots::heatmap.2 I prefer its style and flexibility

# ggheat_longdat <- function(data=export_results, zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), max.month="Jul-2017", title='RR Importation', 
#                    x.angle=45, x.size=10, y.size=NA, leg.title.size=10, leg.text.size=8,
#                    labCol=T, labRow=T, border=F, t.skip=6, leg.key.size=10, na.value="grey60") {
#     
#     
#     
#     ## Check if length of zCuts is even (needs to be)
#     # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
#     
#     ## the function can be be viewed as a two step process
#     ## 1. using the rehape package and other funcs the data is clustered, scaled, and reshaped
#     ## using simple options or by a user supplied function
#     ## 2. with the now resahped data the plot, the chosen labels and plot style are built
#     
#     require(reshape)
#     require(ggplot2)
#     require(RColorBrewer)
#     
#     ## you can either scale by row or column not both! 
#     ## if you wish to scale by both or use a differen scale method then simply supply a scale
#     ## function instead NB scale is a base funct
#     
#     ## First we need to make sure month_yr is an ordered factor, for sorting and limiting
#     data <- set_monthyr_factor(data)
#     
#     
#     ## Second we need to reshape the data into wide format
#     m <- export_results %>% dplyr::select(month_yr, country, region, RR_mean) %>% filter(month_yr < max.month) %>% spread(key=month_yr, value=RR_mean)
#     m$pop <- as.numeric(pop_mean[match(m$country, pop_mean$Country.Code),2])
#     m <- m %>% arrange(desc(region), desc(pop))
#     row.names(m) <- m$country
#     m <- m%>%dplyr::select(-region,-country, -pop)
#     
#     ## this is just reshaping into a ggplot format matrix and making a ggplot layer
#     rows <- dim(m)[1] # state or country
#     cols <- dim(m)[2] # month_yr or year
#     melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), reshape::melt(m))
#     
#     # Start the plot
#     g <- ggplot(data=melt.m)
#     
#     ## add the heat tiles with or without a white border for clarity
#     myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues")))
#     
#     #myPallette <- rev(c(rev(brewer.pal((length(zCuts)/2-1), "YlOrRd")), "white", brewer.pal((length(zCuts)/2-1), "Blues")))
#     cut.intervals <- sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1]))
#     
#     if(border==TRUE){
#         g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
#                                 fill=cut(value, zCuts, include.lowest=T)), colour='white') +
#                 scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
#                           guide = guide_legend(reverse = TRUE))
#     } else {
#         g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
#                                 fill=cut(value, zCuts, include.lowest=T))) +
#         scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
#                           guide = guide_legend(reverse = TRUE))
#     }
#     ## add axis labels either supplied or from the colnames rownames of the matrix
#     if(labCol==T){
#         g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames(m))
#     }else if(labCol==F){
#         g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
#     }else if(labCol=='months'){ 
#         g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames(m)[seq(1,cols,t.skip)])
#         #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
#     }
#     if(labRow==T){ 
#         g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=row.names(m))	
#     } else if(labRow==F){
#         g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
#     }
#     ## get rid of grey panel background and gridlines and tick marks
#     g2 <- g2 + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.border = element_blank(),
#                      panel.background=element_blank(),
#                      axis.ticks = element_blank())
#     
#     ## Move axis text closer to the plot
#     g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
#                      axis.text.x = element_text(margin = margin(t = -20), size=x.size),
#                      legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
#     ## Put the Axis text at an angle for X and size
#     g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
#     
#     # Reduce the space between the legend and the panel & Legend size
#     g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
#                      legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
#                      plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
#                      plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
#                coord_fixed(ratio=aspect_ratio)  
#     
#     return(g2)
# }




## m=matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
## this function makes a graphically appealing heatmap (no dendrogram) using ggplot
## whilst it contains fewer options than gplots::heatmap.2 I prefer its style and flexibility

ggheat_import <- function(data=import_results %>% dplyr::select(month_yr, state, state_name, value=RR_mean), 
                          pop_data=pop_mean,
                          zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), max.month="Jul-2017", title='RR Importation', 
                          x.angle=45, x.size=8, y.size=8, leg.title.size=10, leg.text.size=8, 
                          aspect_ratio=3, axis_margin_y=8,
                          labCol=T, labRow=T, border=F, t.skip=3, leg.key.size=10, na.value="grey60", pal_colors=c("lightsteelblue","navyblue")) {
    
    ## Check if length of zCuts is even (needs to be)
    # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
    
    ## the function can be be viewed as a two step process
    ## 1. using the rehape package and other funcs the data is clustered, scaled, and reshaped
    ## using simple options or by a user supplied function
    ## 2. with the now resahped data the plot, the chosen labels and plot style are built
    
    #require(reshape)
    require(tidyverse)
    require(RColorBrewer)
    
    ## you can either scale by row or column not both! 
    ## if you wish to scale by both or use a differen scale method then simply supply a scale
    ## function instead NB scale is a base funct
    
    ## First we need to make sure month_yr is an ordered factor, for sorting and limiting
    data <- set_monthyr_factor(data)
    
    
    ## Second we need to reshape the data into wide format
    m <- data %>% filter(month_yr <= max.month) %>% spread(key=month_yr, value=value)
    m$pop <- as.numeric(pop_data$pop[match(m$state, pop_data$state)])
    m <- m %>% arrange(desc(pop)) %>% as.data.frame()
    
    # Get state names and months
    row.names(m) <- m$state_name
    m <- m %>% dplyr::select(-state, -state_name, -pop)
    colnames_ <- colnames(m)
    rownames_ <- row.names(m)
    
    # Are NAs in data?
    nas_in_data <- sum(is.na(m))>0
    
    ## this is just reshaping into a ggplot format matrix and making a ggplot layer
    rows <- dim(m)[1] # state or country
    cols <- dim(m)[2] # month_yr or year
    melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), reshape::melt(m))
    melt.m$value[is.na(melt.m$value)] <- -100
    
    
    ## add the heat tiles without a white border for clarity
    if (grepl("RR", title)){
        myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues"), na.value))
    } else {
        myPallette <- rev(c(rev(colorRampPalette(pal_colors)(length(zCuts))), na.value))
    } 
    
    # Interval labels on legend
    if (nas_in_data){
        cut.intervals <- c("NA", sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
        # Add NA values to zCuts
        zCuts_mod <- c(-1001, zCuts)
        if (grepl("Reported", title)){
            cut.intervals <- c("NA", zCuts)
        }
    } else {
        cut.intervals <- c(sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
        # Add NA values to zCuts
        zCuts_mod <- c(zCuts)
        if (grepl("Reported", title)){
            cut.intervals <- c(zCuts)
        }
        myPallette <- myPallette[-1]
    }
    
    
    # Start the plot
    
    g <- ggplot(data=melt.m)
    
    g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
                            fill=cut(value, zCuts_mod, include.lowest=T, right=FALSE))) +
        scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
                          guide = guide_legend(reverse = TRUE))
    
    ## add axis labels either supplied or from the colnames rownames of the matrix
    if(labCol==T){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames_)
    }else if(labCol==F){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
    }else if(labCol=='months'){ 
        g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames_[seq(1,cols,t.skip)])
        #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
    }
    if(labRow==T){ 
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rownames_)	
    } else if(labRow==F){
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
    }
    ## get rid of grey panel background and gridlines and tick marks
    g2 <- g2 + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background=element_blank(),
                     axis.ticks = element_blank(),
                     line = element_blank())
    ## Move axis text closer to the plot
    g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
                     axis.text.x = element_text(margin = margin(t = -20), size=x.size),
                     legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
    ## Put the Axis text at an angle for X and size
    g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
    # Reduce the space between the legend and the panel & Legend size
    g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
                     legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
                     plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
                     plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
        coord_fixed(ratio=aspect_ratio)  
    
    return(g2)
}




ggheat_export <- function(data=import_results %>% dplyr::select(t, source, value=RR_mean), 
                          #pop_data=pop_mean,
                          zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), title='RR Exportation', 
                          x.angle=45, x.size=8, y.size=8, leg.title.size=10, leg.text.size=8, 
                          aspect_ratio=3, axis_margin_y=8,
                          labCol=T, labRow=T, border=F, t.skip=3, leg.key.size=10, na.value="grey60", pal_colors=c("lightsteelblue","navyblue")) {
    
    ## Check if length of zCuts is even (needs to be)
    # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
    
    require(tidyverse)
    require(RColorBrewer)
    
    ## Second we need to reshape the data into wide format
    m <- data %>% spread(key=t, value=value)
    m <- m %>% as.data.frame()
    
    # Get state names and months
    row.names(m) <- m$source
    m <- m %>% dplyr::select(-source)
    colnames_ <- colnames(m)
    rownames_ <- row.names(m)
    
    # Are NAs in data?
    nas_in_data <- sum(is.na(m))>0
    
    ## this is just reshaping into a ggplot format matrix and making a ggplot layer
    rows <- dim(m)[1] # state or country
    cols <- dim(m)[2] # month_yr or year
    melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), reshape::melt(m))
    melt.m$value[is.na(melt.m$value)] <- -100
    
    
    ## add the heat tiles without a white border for clarity
    if (grepl("RR", title)){
        myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues"), na.value))
    } else {
        myPallette <- rev(c(rev(colorRampPalette(pal_colors)(length(zCuts))), na.value))
    } 
    
    # Interval labels on legend
    if (nas_in_data){
        cut.intervals <- c("NA", sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
        # Add NA values to zCuts
        zCuts_mod <- c(-1001, zCuts)
        if (grepl("Reported", title)){
            cut.intervals <- c("NA", zCuts)
        }
    } else {
        cut.intervals <- c(sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
        # Add NA values to zCuts
        zCuts_mod <- c(zCuts)
        if (grepl("Reported", title)){
            cut.intervals <- c(zCuts)
        }
        myPallette <- myPallette[-1]
    }
    
    
    # Start the plot
    
    g <- ggplot(data=melt.m)
    
    g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
                            fill=cut(value, zCuts_mod, include.lowest=T, right=FALSE))) +
        scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
                          guide = guide_legend(reverse = TRUE))
    
    ## add axis labels either supplied or from the colnames rownames of the matrix
    if(labCol==T){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames_)
    }else if(labCol==F){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
    }else if(labCol=='t'){ 
        g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames_[seq(1,cols,t.skip)])
        #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
    }
    if(labRow==T){ 
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rownames_)	
    } else if(labRow==F){
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
    }
    ## get rid of grey panel background and gridlines and tick marks
    g2 <- g2 + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background=element_blank(),
                     axis.ticks = element_blank(),
                     line = element_blank())
    ## Move axis text closer to the plot
    g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
                     axis.text.x = element_text(margin = margin(t = -20), size=x.size),
                     legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
    ## Put the Axis text at an angle for X and size
    g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
    # Reduce the space between the legend and the panel & Legend size
    g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
                     legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
                     plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
                     plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
        coord_fixed(ratio=aspect_ratio)  
    
    return(g2)
}










## m=matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
## this function makes a graphically appealing heatmap (no dendrogram) using ggplot
## whilst it contains fewer options than gplots::heatmap.2 I prefer its style and flexibility

ggheat_export_byregion <- function(data=export_results %>% dplyr::select(t, source, region, value=RR_mean), 
                                   zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), 
                                   max.month="Jul-2017", title='RR Exportation', 
                                   pop_data=pop_mean, 
                                   x.angle=45, x.size=8, y.size=8, leg.title.size=10, leg.text.size=8,
                                   aspect_ratio=3, axis_margin_y=8,
                                   labCol=T, labRow=T, border=F, t.skip=3, leg.key.size=10, na.value="grey60", pal_colors=c("lightsteelblue","navyblue")) {
    
    ## Check if length of zCuts is even (needs to be)
    # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
    
    require(tidyverse)
    require(RColorBrewer)
    
    ## you can either scale by row or column not both! 
    ## if you wish to scale by both or use a differen scale method then simply supply a scale
    ## function instead NB scale is a base funct
    
    ## Second we need to reshape the data into wide format
    m <- data %>% filter(month_yr < max.month) %>% spread(key=t, value=value)
    m$pop <- as.numeric(pop_data[match(m$country, pop_data$iso),"pop"])
    m <- m %>% arrange(desc(region), desc(pop))
    row.names(m) <- m$country_name
    region_ <- m$region
    m <- m %>% dplyr::select(-region,-country, -country_name, -pop)
    
    
    # Adding 2 white spaces between regions in plot
    rowspaces <- 3
    
    colnames_ <- colnames(m)
    rownames_ <- row.names(m)
    regions_unique <- unique(region_)
    maxrows <- sapply(X=regions_unique, FUN=function(x=X) max(which(region_==x)))
    
    row_sapce_value <- -10000
    row_space <- as.data.frame(matrix(as.numeric(rep(row_sapce_value, ncol(m)*rowspaces)), nrow=rowspaces, ncol=ncol(m), dimnames = list(rep("",rowspaces), colnames_)))
    colnames(row_space) <- colnames_
    m_spaced <- bind_rows(m[1:maxrows[1],], # Europe
                          row_space,
                          m[(maxrows[1]+1):maxrows[2],], # Asia
                          row_space,
                          m[(maxrows[2]+1):nrow(m),]) # Africa
    rownames_ <- c(rownames_[1:maxrows[1]], # Europe
                   rep("",rowspaces),
                   rownames_[(maxrows[1]+1):maxrows[2]], # Asia
                   rep("",rowspaces),
                   rownames_[(maxrows[2]+1):nrow(m)])    # Africa 
    
    ## this is just reshaping into a ggplot format matrix and making a ggplot layer
    rows <- dim(m_spaced)[1] # state or country
    cols <- dim(m_spaced)[2] # month_yr or year
    melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), reshape::melt(m_spaced))
    melt.m$value[is.na(melt.m$value)] <- -100
    
    
    ## add the heat tiles without a white border for clarity
    if (grepl("RR", title)){
        myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues"), na.value, "white"))
    } else {
        myPallette <- rev(c(rev(colorRampPalette(pal_colors)(length(zCuts))), na.value, "white"))
    } 
    
    cut.intervals <- c("", "NA", sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
    
    # Add NA values to zCuts
    zCuts_mod <- c(-100001, -1001, zCuts)
    
    
    # Start the plot
    
    g <- ggplot(data=melt.m)
    
    g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
                            fill=cut(value, zCuts_mod, include.lowest=T, right=FALSE))) +
        scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
                          guide = guide_legend(reverse = TRUE))
    
    ## add axis labels either supplied or from the colnames rownames of the matrix
    if(labCol==T){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames_)
    }else if(labCol==F){
        g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
    }else if(labCol=='months'){ 
        g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames_[seq(1,cols,t.skip)])
        #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
    }
    if(labRow==T){ 
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rownames_)	
    } else if(labRow==F){
        g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
    }
    ## get rid of grey panel background and gridlines and tick marks
    g2 <- g2 + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background=element_blank(),
                     axis.ticks = element_blank(),
                     line = element_blank())
    
    ## Move axis text closer to the plot
    g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
                     axis.text.x = element_text(margin = margin(t = -25), size=x.size),
                     legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
    ## Put the Axis text at an angle for X and size
    g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
    
    # Reduce the space between the legend and the panel & Legend size
    g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
                     legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
                     plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
                     plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
        coord_fixed(ratio=aspect_ratio)  
    
    return(g2)
}












# 
# 
# ## m=matrix(data=sample(rnorm(100,mean=0,sd=2)), ncol=10)
# ## this function makes a graphically appealing heatmap (no dendrogram) using ggplot
# ## whilst it contains fewer options than gplots::heatmap.2 I prefer its style and flexibility
# 
# ggheat_export_byregion <- function(data=export_results, zCuts=c(0, .05, .1, .2, .5, .8, 1.25, 2, 5, 10, 20, 40), max.month="Jul-2017", title='RR Importation', 
#                                    x.angle=45, x.size=10, y.size=NA, leg.title.size=10, leg.text.size=8,
#                                    labCol=T, labRow=T, border=F, t.skip=6, leg.key.size=10, na.value="grey60") {
#     
#     ## Check if length of zCuts is even (needs to be)
#     # if(length(zCuts) %% 2 != 0) return("Length of zCuts needs to be even number")
#     
#     ## the function can be be viewed as a two step process
#     ## 1. using the rehape package and other funcs the data is clustered, scaled, and reshaped
#     ## using simple options or by a user supplied function
#     ## 2. with the now resahped data the plot, the chosen labels and plot style are built
#     
#     #require(reshape)
#     require(tidyverse)
#     require(RColorBrewer)
#     
#     ## you can either scale by row or column not both! 
#     ## if you wish to scale by both or use a differen scale method then simply supply a scale
#     ## function instead NB scale is a base funct
#     
#     ## First we need to make sure month_yr is an ordered factor, for sorting and limiting
#     data <- set_monthyr_factor(data)
#     
#     
#     ## Second we need to reshape the data into wide format
#     m <- data %>% dplyr::select(month_yr, country, region, RR_mean) %>% filter(month_yr < max.month) %>% spread(key=month_yr, value=RR_mean)
#     m$pop <- as.numeric(pop_mean[match(m$country, pop_mean$Country.Code),2])
#     m <- m %>% arrange(desc(region), desc(pop))
#     row.names(m) <- m$country
#     region_ <- m$region
#     m <- m %>% dplyr::select(-region,-country, -pop)
#     
#     
#     # Adding 2 white spaces between regions in plot
#     rowspaces <- 3
#     
#     colnames_ <- colnames(m)
#     rownames_ <- row.names(m)
#     regions_unique <- unique(region_)
#     maxrows <- sapply(X=regions_unique, FUN=function(x=X) max(which(region_==x)))
#     
#     # row_space <- as.data.frame(matrix(as.numeric(rep(1, ncol(m)*rowspaces)), nrow=rowspaces, ncol=ncol(m), dimnames = list(rep("",rowspaces), colnames_)))
#     # colnames(row_space) <- colnames_
#     # m_spaced <- bind_rows(m[1:maxrows[1],], # Europe
#     #                       row_space,
#     #                       m[(maxrows[1]+1):maxrows[2],], # Asia
#     #                       row_space,
#     #                       m[(maxrows[2]+1):nrow(m),]) # Africa
#     # rownames_ <- c(rownames_[1:maxrows[1]], # Europe
#     #                rep("",rowspaces),
#     #                rownames_[(maxrows[1]+1):maxrows[2]], # Asia
#     #                rep("",rowspaces),
#     #                rownames_[(maxrows[2]+1):nrow(m)])    # Africa 
#     m_spaced <- m
#     row_regions <- get.region(rownames_)
# 
#     ## this is just reshaping into a ggplot format matrix and making a ggplot layer
#     rows <- dim(m_spaced)[1] # state or country
#     cols <- dim(m_spaced)[2] # month_yr or year
#     melt.m <- cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows), gather(m_spaced))
#     melt.m$value[is.na(melt.m$value)] <- -100
#     melt.m$region <- region_[melt.m$rowInd]
#     ## add the heat tiles with or without a white border for clarity
#     myPallette <- rev(c(rev(brewer.pal((sum(zCuts>1)-1), "YlOrRd")), "white", brewer.pal((sum(zCuts<1)-1), "Blues"), na.value))
#     
#     #myPallette <- rev(c(rev(brewer.pal((length(zCuts)/2-1), "YlOrRd")), "white", brewer.pal((length(zCuts)/2-1), "Blues")))
#     cut.intervals <- c("NA", sapply(X=1:(length(zCuts)-1), FUN=function(x=X) paste0(zCuts[x],'-',zCuts[x+1])))
#     
#     # Add NA values to zCuts
#     zCuts <- c(-1000, zCuts)
#     
#     
#     # Start the plot
#     g <- ggplot(data=melt.m)
#     
#     
#     if(border==TRUE){
#         g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
#                                 fill=cut(value, zCuts, include.lowest=T, right=FALSE)), colour='white') +
#             scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
#                               guide = guide_legend(reverse = TRUE))
#     } else {
#         g2 <- g + geom_rect(aes(xmin=colInd-1, xmax=colInd, ymin=rowInd-1, ymax=rowInd, 
#                                 fill=cut(value, zCuts, include.lowest=T, right=FALSE))) +
#             scale_fill_manual(title, values = myPallette, labels=cut.intervals, drop = FALSE, na.value=na.value,
#                               guide = guide_legend(reverse = TRUE))
#     }
#     ## add axis labels either supplied or from the colnames rownames of the matrix
#     if(labCol==T){
#         g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames_)
#     }else if(labCol==F){
#         g2 <- g2 + scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
#     }else if(labCol=='months'){ 
#         g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,t.skip), labels=colnames_[seq(1,cols,t.skip)])
#         #g2 <- g2 + scale_x_continuous(breaks=seq(1,cols,12), labels=substr(colnames(m),5,8)[seq(1,cols,12)]) # Only print years
#     }
#     if(labRow==T){ 
#         g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rownames_)	
#     } else if(labRow==F){
#         g2 <- g2 + scale_y_continuous(trans = "reverse", breaks=(1:rows)-0.5, labels=rep('',rows))	
#     }
#     ## get rid of grey panel background and gridlines and tick marks
#     g2 <- g2 + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.border = element_blank(),
#                      panel.background=element_blank(),
#                      axis.ticks = element_blank())
#     
#     g2 <- g2 + facet_grid(desc(region) ~ .)
#     
#     ## Move axis text closer to the plot
#     g2 <- g2 + theme(axis.text.y = element_text(margin = margin(r = -axis_margin_y), size=y.size),
#                      axis.text.x = element_text(margin = margin(t = -20), size=x.size),
#                      legend.title=element_text(size=leg.title.size) , legend.text=element_text(size=leg.text.size)) # legend text size
#     ## Put the Axis text at an angle for X and size
#     g2 <- g2 + theme(axis.text.x = element_text(angle = x.angle, hjust = 1))
#     
#     # Reduce the space between the legend and the panel & Legend size
#     g2 <- g2 + theme(legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-20,-20,-10),
#                      legend.key.size=unit(leg.key.size, "pt"), # Change key size in the legend 
#                      plot.title=element_text(size=10, vjust=-5), # Adjust title size and vertical spacing
#                      plot.margin = unit(c(0,1,.25,.25), "cm")) + # Adjust plot margins
#               coord_fixed(ratio=aspect_ratio)  
#     
#     return(g2)
# }

