CleanUp <- function(dir){
  if(file.exists(file.path(dir,"temp_dir"))) unlink(file.path(dir,"temp_dir"),recursive = TRUE, force = TRUE)
  files <- list.files(dir, 
                      full.names = TRUE, 
                      recursive = TRUE) 
  Savedfiles <- files[grep("[r,R]un.+\\.mod", files)]
  files <-  files[! files %in% Savedfiles]
  # add back the npde control files from update_inits, to be removed
  files <- c(files,"RunNPDE.mod.org")
  for(file in files){
    if(file.exists(file)) file.remove(file)
  }
  
} 