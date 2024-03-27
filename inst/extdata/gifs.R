rm(list = ls())
source("RCore/PredictivePedestrian.R")


# Saves out animated gif, invisibly returns
# dName: directory where pdfs are located
# outname: name of output gif
makeGifs <- function(dName, outNam, figs = "all", density = 45, fps = 4) {
  
  # Read in a bunch of pdfs, density sets size
  getPDFs <- function(dName =".", density = 45) {
    iNames <- dir(dName, pattern = "*.pdf")
    
    # Put in numeric order
    iNames <- iNames[order(as.numeric(unlist(lapply(
      strsplit(unlist(strsplit(iNames, ".pdf")), "_N"), function(x) {
        x[2]
      }))))]
    
    # Only keep selection of PDFs for testing
    if (figs != "all") {
      iNames <- iNames[figs]
    }
    
    movie <- image_read_pdf(paste(dName, iNames[[1]], sep = "/"), 
                            density = density)
    for (i in iNames[-1]) {
      movie <- c(movie, image_read_pdf(paste(dName, i, sep = "/"), 
                                       density = density))
    }
    movie
  }
  
  # Read PDFs and order them correctly
  pdfs <- getPDFs(dName, density = density)
  
  # Combine frames
  gifs  <- image_animate(pdfs, fps = fps)
  
  # Save gif
  image_write(gifs, path = outNam, format = "gif") 
  invisible(pdfs)
}

exp_id <- "Supermarket"  # experiment id
m_id <- "m01"            # m[make id]
s_id <- "s12"            # s[stack id]
p_id <- "p16"            # p[play id]
r_id <- "r002"           # r[rep id] or ""
figs_s <- "all"          # start frame "all", or e.g., 1
figs_e <- ""             # end frame "" (if all) or e.g. 1000

if (figs_s == "all") {
  figs <- "all"
} else {
  figs <- figs_s:figs_e
}

nam <- paste(tolower(exp_id), "_", sep = "")
play_nam <- paste("3-play/play_", nam, m_id, s_id, p_id, r_id, sep = "")
fig_dir <- paste("Experiments/", exp_id, "/Figures", sep = "")

plot_dir <- paste(fig_dir, "/", play_nam, sep = "")

makeGifs(dName = plot_dir, 
         outNam = paste(plot_dir, "_", figs_s, "_", figs_e, ".gif", sep = ""), 
         figs = figs,
         fps = 10)

