
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ade4)
library(RColorBrewer)
library(MASS)
library(plotrix)

shinyServer(function(input, output, session){
  
  ### Functions ####
  
  update_colors <- function(input){
    inFile <- input$file1
    input_dat <- read.csv(inFile$datapath, header = TRUE, sep = input$sep, dec = input$dec)
    x<-colnames(input_dat)
    x<-x[!(x %in% c("LA", "Nmass", "LMA", "H", "SM", "SSD"))]
    updateSelectInput(session, "colors",
                      label = "Select a column for colors",
                      choices = c("none", x),
                      selected = "none" )
  }
  
  check_species <- function(input, list.sp){
    if(length(list.sp$sp) > 0){
      straits <- c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM)
      traits<- c("H", "SSD", "LA", "LMA", "Nmass", "SM")[straits]
      sGF <- c(input$other, input$herb, input$other, input$other, input$shrub, input$tree, input$other)
      gf <- c("Aquatic", "Herb", "Liana", "Non-woody epiphyte", "Shrub", "Tree", "Vine")[sGF]
      colblob <- c("LOG10.Height.", "LOG10.SSD.", "LOG10.LA.", "LOG10.LMA.", "LOG10.Nmass.", "LOG10.Seed.Mass.")[straits]
      dat    <- read.csv("data/Blob&GF.csv", header = TRUE, sep = ";", dec = ".")
      Blob_sp <- as.character(dat$name_TLP_TRY30_resolved[which(dat$GF %in% gf)])
      list.sp$sp <- list.sp$sp[list.sp$sp %in% Blob_sp]
      if(length(list.sp$sp) == 0) list.sp$sp <- NULL
    }
    return(list.sp)
  }
  
  check_variables <- function(input){
    return(sum(c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM)))
  }
  
  plotPCA <- function(input = input, list.sp = list.sp, boxplot = TRUE){
    
    # Inputs customized ####
    straits <- c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM)
    traits<- c("H", "SSD", "LA", "LMA", "Nmass", "SM")[straits]
    sGF <- c(input$other, input$herb, input$other, input$other, input$shrub, input$tree, input$other)
    gf <- c("Aquatic", "Herb", "Liana", "Non-woody epiphyte", "Shrub", "Tree", "Vine")[sGF]
    colblob <- c("LOG10.Height.", "LOG10.SSD.", "LOG10.LA.", "LOG10.LMA.", "LOG10.Nmass.", "LOG10.Seed.Mass.")[straits]
    dat    <- read.csv("data/Blob&GF.csv", header = TRUE, sep = ";", dec = ".")
    Blob_GF <- as.character(dat$GF[which(dat$GF %in% gf)])
    Blob_sp <- as.character(dat$name_TLP_TRY30_resolved[which(dat$GF %in% gf)])
    dat <- dat[which(dat$GF %in% gf), colblob, drop = FALSE]
    
    limit <- list("H" = c(0.01, 0.1, 1, 10, 100),
                  "SSD" = c(0.05, 0.1, 0.2, 0.5, 1),
                  "LA" = c(1, 100, 10000, 1000000),
                  "LMA" = c(10, 20, 50, 100, 200, 500, 1000),
                  "Nmass" = c(5, 10, 20, 50),
                  "SM" = c(10^-5, 10^-2, 10, 10^4))
    
    if(nrow(dat) > 0){
      if(ncol(dat) > 2){
        type <- "PCA"
        PCA <- ade4::dudi.pca(dat, center = T, scale = T, scannf = F, nf = ncol(dat))
        means<- PCA$cent
        sds <- PCA$norm
        eigen <- round(PCA$eig / sum(PCA$eig) * 100, 1)
      }else{
        if(ncol(dat) == 2){
          type <- "Biplot"
        }else{
          type <- "None"  
        }
      }
    } else {
      type <- "None"
    }
    
    # Inputs dataset ####
    inFile <- input$file1
    if (!is.null(inFile)){
      input_dat <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec)
      if (all(traits %in% colnames(input_dat))){
        if(all(apply(input_dat[, traits], MARGIN = 2, is.numeric))){
          if(all(unlist(lapply(input_dat[, traits], min, na.rm = TRUE)) > 0)){
            if(input$colors %in% c("NA", "none")){
              col <- brewer.pal(3, name = "Set1")[2]
              leg <- "none"
            }else{
              na_dat <- apply(as.matrix(input_dat[, traits]), MARGIN = 1, function(x){any(is.na(x))})
              dat_col <- input_dat[!na_dat, input$colors]
              if (is.numeric(dat_col)){
                breaks <- cut(dat_col, 10)
                selectedcol <- colorRampPalette(RColorBrewer::brewer.pal(9, name = "Blues"))(10)
                col <- selectedcol[breaks]
                leg <- "degr"
              } else {
                dat_col <- droplevels(as.factor(dat_col))
                if (nlevels(dat_col) < 8){
                  selectedcol <- brewer.pal(9, name = "Set1")[-1]
                  col <- selectedcol[dat_col]
                  leg <- "dis"
                }else{
                  selectedcol <- colorRampPalette(RColorBrewer::brewer.pal(9, name = "Blues"))(nlevels(dat_col))
                  col <- selectedcol[dat_col]
                  leg <- "toomuch"
                }
              }
            }
            input_dat <- as.matrix(input_dat[, traits])
            input_dat <- na.omit(input_dat)
          } else {
            inFile <- NULL
          }
        } else {
          inFile <- NULL
        }
      }else{
        inFile <- NULL
      }
    }
    
    titre <- c("H~(m)", "SSD~(mg.mm^-3)", "LA~(mm^2)", "LMA~(g.m^-2)",
               "Nmass~(mg.g^-1)", "SM~(mg)")[straits]
    
    # boxplots ####
    if(boxplot & type != "None"){
      layout(rbind(c(1:length(traits)), matrix(length(traits) + 1, 4, length(traits))))
      par(mar = c(1, 3.5, 2, 0.1))
      
      for (i in 1:length(traits)){
        if (!is.null(inFile)){
          if(input$colors %in% c("NA", "none") | leg == "degr"){
            boxplot(10^dat[, i], xlim = c(0.5, 2.5), log = "y", main = parse(text = titre[i]),
                    las = 2, outline = FALSE, col = "grey75",
                    ylim = c(min(c(10^dat[, i], input_dat[, i]), na.rm = TRUE), 
                             max(c(10^dat[, i], input_dat[, i]), na.rm = TRUE)))
            boxplot(input_dat[, i], add = TRUE, border = 1, col = brewer.pal(3, name = "Set1")[2], at = 2, yaxt = "n")
          }else{
            boxplot(10^dat[, i], xlim= c(0.5, nlevels(dat_col) + 1.5), log = "y", main = parse(text = titre[i]),
                    las = 2, outline = FALSE, col = "grey75",
                    ylim = c(min(c(10^dat[, i], input_dat[, i]), na.rm = TRUE),
                             max(c(10^dat[, i], input_dat[, i]), na.rm = TRUE)))
            for (j in 1:nlevels(dat_col)) boxplot(input_dat[dat_col == levels(dat_col)[j], i], 
                                                  add = TRUE, border = 1, lwd = .5, col = selectedcol[j],
                                                  at = j + 1, yaxt="n")
          }
        } else{
          boxplot(10^dat[, i], xlim = c(0.5, 1.5), log = "y", main = parse(text = titre[i]),
                  las = 2, outline = FALSE, col = "grey75",
                  ylim = c(min(10^dat[, i]), max(10^dat[, i])))
        }
        if(length(list.sp$sp) > 0){
          sp_sort <- sort(list.sp$sp)
          ligne <- match(sp_sort, Blob_sp)
          val <- sort(10^dat[ligne, i])
          num <- (1:length(list.sp$sp))[order(dat[ligne, i])]
          segments(rep(1, length(val)), val, 
                   rep(c(0.7, 1.3), length.out = length(val)), val)
          points(rep(1, length(val)), val, pch = 21, bg = "white", cex = 1.5)
          text(rep(c(0.6, 1.4), length.out = length(val)), val, num, cex = 1.2)
        }
      }
    }
    
    # draw plot ####
    par(mar = c(5, 5, 0.1, 0.1), cex = 1.5)
    if(type == "PCA"){
      Axis1 <- which(input$axis1 == c("PC1", "PC2", "PC3", "PC4", "PC5"))
      Axis2 <- which(input$axis2 == c("PC2", "PC3", "PC4", "PC5", "PC6")) + 1
      signx <- ifelse(sign(sum(sign(PCA$c1[, Axis1]))) < 0, -1 ,1)
      signy <- ifelse(sign(sum(sign(PCA$c1[, Axis2]))) < 0, -1, 1)
      
      x <- signx * PCA$li[, Axis1]
      y <- signy * PCA$li[, Axis2]
      xlim <- c(-6, 6)
      ylim <- c(-6, 6)
      xlab <- paste0(input$axis1, " (", eigen[Axis1], "%)")
      ylab <- paste0(input$axis2, " (", eigen[Axis2], "%)")
      lims <- c(c(-7, 7), c(-7, 7))
    } else {
      if(type == "Biplot"){
        x <- dat[, 1]
        y <- dat[, 2]
        xlim <- c(min(x) - 0.1 * (max(x) - min(x)),
                  max(x) + 0.1 * (max(x) - min(x)))
        ylim <- c(min(y) - 0.1 * (max(y) - min(y)),
                  max(y) + 0.1 * (max(y) - min(y)))
        xlab <- parse(text = titre[1])
        ylab <- parse(text = titre[2])
        lims <- c(xlim, ylim)
      }
    }
    
    if(type != "None"){
      plot(x, y, ylim = ylim, xlim = xlim, xlab = xlab, ylab = ylab, t = "n", axes = F)
      box()
      if(type == "PCA"){
        axis(1); axis(2)
      } else {
        axis(1, at = log10(limit[[traits[1]]]), labels = limit[[traits[1]]])
        axis(2, at = log10(limit[[traits[2]]]), labels = limit[[traits[2]]], las = 2)
      }
      
      #Ajout des couleurs ####
      if(input$theme == "All species"){
        z <- MASS::kde2d(x, y, lims = lims, n = 100)
        dx <- diff(z$x[1:2]) 
        dy <- diff(z$y[1:2]) 
        sz <- sort(z$z) 
        c1 <- cumsum(sz) * dx * dy 
        prob <- c(0.99, 0.95, 0.5)
        levelsc <- sapply(prob, function(x) {approx(c1, sz, xout = 1 - x)$y})
        probd <- seq(0.99, 0.01, -0.01)
        levelsd <- sapply(probd, function(x) {approx(c1, sz, xout = 1 - x)$y})
        
        cold <- colorRampPalette(colors = c("white", "gold", "red"), bias = 1.5)(length(levelsd))
        cont <- contourLines(z, levels = levelsd)
        
        for (i in 1:length(cont)){
          lev <- which(cont[[i]]$level == levelsd)
          polygon(cont[[i]]$x, cont[[i]]$y, col = cold[lev], border = NA)
        }
        contour(z, labels = prob, levels = round(levelsc, 7), add = T, col = "grey20", labcex = 1)
      } else {
        colGF <- c(Herb = "#4DAF4A4D", Shrub = "#377EB84D", Tree = "#E41A1C4D")
        colGF2 <- c(Herb = "#4DAF4A", Shrub = "#377EB8", Tree = "#E41A1C")
        selec <- c((input$theme == "Growth forms" | input$theme == "Shrubs only") & any(Blob_GF == "Shrub"),
                   (input$theme == "Growth forms" | input$theme == "Herbs only") & any(Blob_GF == "Herb"),
                   (input$theme == "Growth forms" | input$theme == "Trees only") & any(Blob_GF == "Tree"))
        if (any(selec)){
          for(i in c("Shrub", "Herb", "Tree")[selec]){
            z <- MASS::kde2d(x[Blob_GF == i], y[Blob_GF == i], lims = lims, n = 100)
            prob <- c(0.95, 0.5)
            dx <- diff(z$x[1:2]) 
            dy <- diff(z$y[1:2]) 
            sz <- sort(z$z) 
            c1 <- cumsum(sz) * dx * dy 
            levelsc <- sapply(prob, function(x) {approx(c1, sz, xout = 1 - x)$y})
            cont <- contourLines(z, levels = levelsc)
            for (j in 1:length(cont)){
              lev <- which(cont[[j]]$level == levelsc)
              polygon(cont[[j]]$x, cont[[j]]$y, col = colGF[i], border = NA)
            }
            contour(z, labels = prob, levels = round(levelsc, 7), add = T, col = colGF2[i], labcex = 1)
          }
          legend("topleft", legend = c("Herbs", "Shrubs", "Trees")[selec[c(2,1,3)]], fill = colGF[selec[c(2,1,3)]], cex = .8)
        }
      }
      
      # Points et arrows ####
      points(x, y, pch = 16, cex = .2, col = rgb(0, 0, 0, alpha = .3))
      if(type == "PCA"){
        mult <- 5.5
        arrows(0, 0, signx * PCA$co[, Axis1] * mult, signy * PCA$co[, Axis2] * mult,
               lwd = 1.5, length = .08)
        draw.ellipse(0, 0, a = mult, b = mult, lty = 2, border = "grey75")
        
        pos <- NULL
        for (j in 1:nrow(PCA$co)){
          a <- PCA$co[j, c(Axis1, Axis2)]
          if(which.max(abs(a)) == 1) b <- ifelse(signx * a[1] < 0, 2, 4)
          if(which.max(abs(a)) == 2) b <- ifelse(signy * a[2] < 0, 1, 3)
          pos <- c(pos, b)
        }
        
        dep <- cbind(c(0, .01), c(.01, .01), c(.01, 0),
                     c(.01, -.01), c(0, -.01), c(-.01, -.01),
                     c(-.01, 0), c(.01, -.01))
        for(z in 1:8){
          text(signx * PCA$co[, Axis1] * mult + dep[1, z],
               signy * PCA$co[, Axis2] * mult + dep[2, z],
               traits, pos = pos, cex = 0.8,
               col = par("bg"), font = 2)
        }
        text(signx * PCA$co[, Axis1] * mult,
             signy * PCA$co[, Axis2] * mult,
             traits, pos = pos, cex = 0.8)
      }
      
      if(type =="Biplot"){
        test <- cor.test(x, y)
        pval <- ifelse(test$p.value < 0.001, "***", 
                       ifelse(test$p.value < 0.01, "**", 
                              ifelse(test$p.value < 0.05, "*", 
                                     ifelse(test$p.value < 0.1, ".", "ns"))))
        mtext(text = paste("corr = ", round(test$estimate^2, 2), pval), side = 3, line = -1.5, adj = .99, cex = 1.5)
      }
      
      # Draw infile points ####
      if (!is.null(inFile)){
        if(type == "PCA"){
          input_dat <- t(apply(input_dat, MARGIN = 1, function(X){(log10(X) - means) / sds}))
          xcoord <- input_dat %*% matrix(signx * PCA$c1[, Axis1], length(traits), 1)
          ycoord <- input_dat %*% matrix(signy * PCA$c1[, Axis2], length(traits), 1)
        } else {
          xcoord <- log10(input_dat[, traits[1]])
          ycoord <- log10(input_dat[, traits[2]])
        }
        points(xcoord, ycoord, pch = 21, cex = 1, bg = col)
        if (leg %in% c("dis", "toomuch")){
          legend("bottomleft", levels(dat_col), pch = 21, pt.bg = selectedcol, ncol = floor(nlevels(dat_col) / 9) + 1, cex = 1)
        } else if(leg == "degr"){
          grad <- seq(xlim[1], xlim[1] + 0.3 * diff(xlim), length.out = 10)
          rect(grad, ylim[1],
               grad + (grad[2] - grad[1]), ylim[1] + 0.02 * diff(ylim),
               col = selectedcol)
          b <- levels(breaks)
          values <- rowMeans(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", b) ),
                                   upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", b) )))
          text(grad + (grad[2] - grad[1]) / 2, rep(ylim[1] + 0.01 * diff(ylim), 10), values, cex = .6, pos = rep(c(3, 1), 5))
          text(grad[6], ylim[1] + 0.08 * diff(ylim), input$colors)
        }
      }
      # Draw selected species ####
      
      if(length(list.sp$sp) > 0){
        sp_sort <- sort(list.sp$sp)
        ligne <- match(sp_sort, Blob_sp)
        points(x[ligne], y[ligne], pch = 21, bg = "white", cex = 2)
        text(x[ligne], y[ligne], 1:length(sp_sort), cex = .7)
      }
    }
  }
  
  csv <- function(input = input){
    if(!is.null(input$file1)){
      # Inputs customized ####
      straits <- c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM)
      traits<- c("H", "SSD", "LA", "LMA", "Nmass", "SM")[straits]
      sGF <- c(input$other, input$herb, input$other, input$other, input$shrub, input$tree, input$other)
      gf <- c("Aquatic", "Herb", "Liana", "Non-woody epiphyte", "Shrub", "Tree", "Vine")[sGF]
      Growth_form <- c("herbaceous (n=1166)", "shrub (n=173)", "tree (n=846)", "other (n=29)")[c(input$herb, input$shrub, input$tree, input$other)]
      if(length(Growth_form) == 4) Growth_form <- "All (n = 2214)"
      colblob <- c("LOG10.Height.", "LOG10.SSD.", "LOG10.LA.", "LOG10.LMA.", "LOG10.Nmass.", "LOG10.Seed.Mass.")[straits]
      dat    <- read.csv("data/Blob&GF.csv", header = TRUE, sep = ";", dec = ".")
      Blob_sp <- as.character(dat$name_TLP_TRY30_resolved[which(dat$GF %in% gf)])
      dat <- dat[which(dat$GF %in% gf), colblob]
      
      if(ncol(dat) > 2){
        PCA <- ade4::dudi.pca(dat, center = TRUE, scale = TRUE, scannf = F, nf = ncol(dat))
        means<- PCA$cent
        sds <-PCA$norm
        eigen <- round(PCA$eig / sum(PCA$eig) * 100, 1)
      }else{
        return(NULL)
      }
      
      # Inputs dataset ####
      inFile <- input$file1
      input_dat <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec)
      if (all(traits %in% colnames(input_dat))){
        if(all(apply(input_dat[, traits], MARGIN = 2, is.numeric))){
          na_dat <- apply(as.matrix(input_dat[, traits]), MARGIN = 1, function(x){any(is.na(x))})
          input_dat2 <- as.matrix(input_dat[, traits])
          input_dat2 <- na.omit(input_dat2)
        } else {
          return(NULL)
        }
      }else{
        return(NULL)
      }
      sign <- ifelse(sign(colSums(sign(PCA$c1))) < 0, -1 ,1)
      input_dat_cs <- t(apply(input_dat2, MARGIN = 1, function(X){(log10(X) - means) / sds}))
      coord <- input_dat_cs %*% t(sign*t(PCA$c1))
      csv <- data.frame(input_dat, matrix(NA, nrow(input_dat), length(traits)))
      colnames(csv)[(ncol(csv) - length(traits) + 1) : ncol(csv)] <- paste0("PC", 1:length(traits))
      csv[!na_dat, (ncol(csv) - length(traits) + 1) : ncol(csv)] <- coord
      entete <- paste0("This file was automatically produced by the PhenoSpace shiny app.\nIt contains coordinates in the phenotypic space computed in the 'Global Spectrum of Plant Form and Function' - Nature 529, 167-171 (2016)\nbased on ", paste(traits, collapse = ", "), " values for ", paste(Growth_form, collapse = ", "), " species")
      entete <- c(entete, rep("", ncol(csv) - 1))
      csv <- rbind(entete, rep("", ncol(csv)), colnames(csv), as.matrix(csv))
      return(csv)
    } else {
      return(NULL)
    }
  }
  
  ### Observe ####
  
  observeEvent(c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM),
               {updateSelectInput(session = session, inputId = "axis1", choices = paste0("PC", 1:(check_variables(input)-1)))
                 updateSelectInput(session = session, inputId = "axis2", choices = paste0("PC", 2:check_variables(input)))})
  
  list.sp <- reactiveValues(sp = NULL)
  
  observeEvent(input$species, if (!input$species %in% c("NA", "...")) list.sp$sp <- c(list.sp$sp, input$species))
  
  observeEvent(input$file1, if(!is.null(input$file1)) update_colors(input))
  
  observeEvent(input$sep, if(!is.null(input$file1)) update_colors(input))
  
  observeEvent(input$find, if (input$find != ""){
    sGF <- c(input$other, input$herb, input$other, input$other, input$shrub, input$tree, input$other)
    gf <- c("Aquatic", "Herb", "Liana", "Non-woody epiphyte", "Shrub", "Tree", "Vine")[sGF]
    dat    <- read.csv("data/Blob&GF.csv", header = TRUE, sep = ";", dec = ".")
    Blob_sp <- as.character(dat$name_TLP_TRY30_resolved[which(dat$GF %in% gf)])
    SP <- Blob_sp[grep(tolower(input$find), tolower(Blob_sp))]
    updateSelectInput(session, "species",
                      label = "... select a species name:",
                      choices = c(ifelse(identical(SP, character(0)), "NA", "..."), SP),
                      selected = ifelse(identical(SP, character(0)), "NA", "...") )
  })
  
  observeEvent(input$selec_plot, 
               if (input$selec_plot == "PCA only"){
                 updateTextInput(session, "h_plot", "Height: ", 22)
               } else {
                 updateTextInput(session, "h_plot", "Height: ", 26)
               }
  )
  
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
  observe({list.sp <- check_species(input = input, list.sp = list.sp)})
  
  observeEvent(input$reset, list.sp$sp <- NULL)
  
  ### Outputs ####
  
  output$PCAPlot <- renderPlot({
    plotPCA(input = input, list.sp = list.sp)
  })
  
  output$upload <- renderText({
    straits <- c(input$LA, input$Nmass, input$LMA, input$H, input$SM, input$SSD)
    traits <- c("LA", "Nmass", "LMA", "H", "SM", "SSD")[straits]
    inFile <- input$file1
    
    if (!is.null(inFile)){
      input_dat <- read.csv(inFile$datapath, header = TRUE, sep = input$sep, dec = input$dec)
      if (all(traits %in% colnames(input_dat))){
        input_dat <- input_dat[, traits]
        if (all(unlist(lapply(input_dat, is.numeric)))){
          if (any(unlist(lapply(input_dat, min, na.rm = TRUE)) < 0)){
            return(paste0("<b>Non-positive values found in ",
                          colnames(input_dat)[unlist(lapply(input_dat, min, na.rm = TRUE)) < 0],
                          "</b>"))
          } else{
            return(paste0("<b>All selected traits found in the uploaded file for ",
                          nrow(na.omit(input_dat)), " rows. ",
                          nrow(input_dat) - nrow(na.omit(input_dat))," incomplete rows omitted.</b>"))
          }
        }else{
          return(paste0("<b>Non-numeric values in ",
                        paste0(traits[!unlist(lapply(input_dat, is.numeric))], collapse = ", "),
                        ". Please select the correct decimal character below or check your csv file.</b>"))
        }
      }else{
        if(any(traits %in% colnames(input_dat))){
          return(paste0("<b>", paste(traits[!traits %in% colnames(input_dat)], collapse = ", "),
                        " not found in the uploaded file. Please check your csv file or unselect these traits in the 'Customize the PCA' tab.</b>"))
        }else{
          return(paste0("<b>Bad result, please select the correct separator character below or check your csv file.</b>"))
        }
      }
    }else{
      return("")
    }
  })
  
  output$file <- reactive({if(is.null(input$file1)) FALSE else TRUE})
  outputOptions(output, 'file', suspendWhenHidden = FALSE)
  
  output$meme <- reactive({straits <- c(input$H, input$SSD, input$LA, input$LMA, input$Nmass, input$SM)
  sGF <- c(input$other, input$herb, input$shrub, input$tree)
  if(sum(straits) < 2 | sum(sGF) == 0) TRUE else FALSE})
  outputOptions(output, 'meme', suspendWhenHidden = FALSE)
  
  output$sp <- renderText({
    if(!is.null(list.sp$sp)){
      sp_sort <- sort(list.sp$sp)
      textsp <- paste(1:length(sp_sort), sp_sort, sep = ": ", collapse = "; ")
      return(paste0("<b>Species in the original data set from Díaz <i>et al.</i> (2016): </b> ",textsp))
    }else{
      return("")
    }
  })
  
  ### Download figure ####
  
  output$downplot <- downloadHandler(
    filename = function() {paste0(input$name_plot, ".", input$format_plot)},
    content = function(file) {
      width <- as.numeric(input$w_plot) * ifelse(input$unit_plot == "cm", 1, 2.54)
      height <- as.numeric(input$h_plot) * ifelse(input$unit_plot == "cm", 1, 2.54)
      
      if(input$format_plot == "png"){
        png(filename = file, 
            width = width, height = height,
            res = as.numeric(input$res_plot), units = "cm")
      }
      
      if(input$format_plot == "pdf"){
        pdf(file = file, 
            width = as.numeric(input$w_plot) / 2.54, 
            height = as.numeric(input$h_plot) / 2.54) 
      }
      
      if(input$format_plot == "svg"){
        svg(filename = file, 
            width = as.numeric(input$w_plot) / 2.54, 
            height = as.numeric(input$h_plot) / 2.54)
      }
      
      if(input$selec_plot == "PCA only"){
        plotPCA(input = input, list.sp = list.sp, boxplot = FALSE)
      } else {
        plotPCA(input = input, list.sp = list.sp) 
      }
      dev.off()
    }
  )
  
  ### Download csv ####
  
  output$downcsv <- downloadHandler(
    filename = function() {paste0(input$name_csv, ".csv")},
    content = function(file) {
      write.table(x = csv(input), file = file, sep = input$sep_csv, dec = input$dec_csv, col.names = F, row.names = F, quote = F)
    }
  )
})