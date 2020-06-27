mat.tool <- function(proceed = NULL, engine = NULL, path = NULL) {
  if (is.null(proceed)) proceed <- readline("The function will change system-level settings. Do you want to proceed? (Yes/no/cancel)")
  if (!proceed %in% c("YES", "Y", "y", "yes", "Yes")) stop ("The function is stopped")
  sys.name <- Sys.info()["sysname"]
  if (sys.name == "Linux") {
    linux.type <- system("grep ID_LIKE /etc/*-release", intern=TRUE)
    if (grepl("debian", linux.type)) sys.name <- "Debian"
  }
  if (sys.name != "Darwin" & sys.name != "Debian") stop ("The current version of the function only supports Mac OS and Debian Linux")
  currrent.blas.path <- file.path(extSoftVersion()["BLAS"])
  blas.lib <- basename(currrent.blas.path)
  if (grepl("openblas", currrent.blas.path) | grepl("mkl", currrent.blas.path)) {
    stop("R is already linked with a optimized BLAS library.")
  }
  # For OS X
  if (sys.name == "Darwin") {
    # Back up
    backup.file <- paste(currrent.blas.path, "backup", sep = ".")
    if (!file.exists(backup.file)) {
      backup.cmd <- paste('yes "y" | cp', currrent.blas.path, backup.file, sep = " ")
      invisible(system(backup.cmd, intern = TRUE))
      message("Rblas is backed up")
    }
    # If path is supplied
    if (!is.null(path)) {
      if (! grepl("libmkl_rt.dylib", path) | grepl("openblas/lib/libblas.dylib", path)) {
        stop("Invalid path.")
      }
      if (grepl("openblas", path)) {
        openblas.o.dir <- path
      } else if (grepl("mkl", path)) {
        mkl.o.dir <- path}
    } else {
      mkl.o.dir <- "/opt/intel/mkl/lib/libmkl_rt.dylib"
      openblas.o.dir <- "/usr/local/opt/openblas/lib/libblas.dylib"
    }
    if (blas.lib == "libBLAS.dylib") stop ("R is linked with system BLAS library.")
    # Automatically detect optimized library
    if (is.null(engine)) {
      message("No engine is supplied. Will detect automatically.")
      if (file.exists(mkl.o.dir)){
        engine <- "MKL"
        message("Intel MKL is detected. Will use Intel MKL as the optimized BLAS library.")
      } else if (file.exists(openblas.o.dir)){
        engine <- "OpenBLAS"
        openblas.dir <- openblas.o.dir
        message("OpenBLAS is detected. Will use OpenBLAS as the optimized BLAS library.")
      } else stop("No optimized BLAS library is detected. The BLAS library is not changed. The package currently only supports Intel MKL and OpenBLAS. Install a optimized BLAS library or supply a custom path.")
    }

    # Create a working copy of Rblas
    cp.file <- paste(currrent.blas.path, "keep", sep = ".")
    cp.cmd <- paste('yes "y" | cp', currrent.blas.path, cp.file, sep = " ")
    invisible(system(cp.cmd, intern = TRUE))

    # Symbolic link
    if (engine == "MKL"){
      mkl.dir <- mkl.o.dir
      link.cmd <- paste('ln -sf', mkl.dir, currrent.blas.path)
      message("R is linked with Intel MKL")}
    if (engine == "OpenBLAS"){
      openblas.dir <- openblas.o.dir
      link.cmd <- paste('ln -sf', openblas.dir, currrent.blas.path)
      message("R is linked with OpenBLAS")}
    invisible(system(link.cmd, intern = TRUE))
  }
  # For Linux
  if (sys.name == "Debian") {
    blas.list <- system("update-alternatives --list libblas.so.3-x86_64-linux-gnu", intern = TRUE)
    mkl.no <- NA
    openblas.no <- NA
    for (i in 1:length(blas.list)) {
      if (grepl("openblas", blas.list[i])) openblas.no <- i
      if (grepl("mkl", blas.list[i])) mkl.no <- i
    }

    # Automatically detect optimized library
    if (is.null(engine)) {
      message("No engine is supplied. Will detect automatically.")
      if (!is.na(mkl.no)) {
        engine <- "MKL"
        message("Intel MKL is detected. Will use Intel MKL as the optimized BLAS library.")
      } else if (!is.na(openblas.no)) {
        engine <- "OpenBLAS"
        message("OpenBLAS is detected. Will use OpenBLAS as the optimized BLAS library.")
      } else stop("No optimized BLAS library is detected. The package currently only supports Intel MKL and OpenBLAS.")
    }
    if (!is.null(path)) message(" 'path' is ignored on Linux systems")
    if (engine == "MKL"){
      new.blas.no <- mkl.no
      link.cmd1 <- paste("sudo -kS update-alternatives --config libblas.so.3-x86_64-linux-gnu")
      link.cmd2 <- paste("sudo -kS update-alternatives --config liblapack.so.3-x86_64-linux-gnu")
    }
    if (engine == "OpenBLAS"){
      new.blas.no <- openblas.no
      link.cmd1 <- paste("sudo -kS update-alternatives --config libblas.so.3-x86_64-linux-gnu")
      link.cmd2 <- paste("sudo -kS update-alternatives --config liblapack.so.3-x86_64-linux-gnu")
    }
    invisible(system(link.cmd1, intern=TRUE, ignore.stderr = TRUE,
      input=c(readline("To change system setting requires superuser password. Please enter the password if you want to proceed: "), paste(new.blas.no))))
    invisible(system(link.cmd1, intern=TRUE, ignore.stderr = TRUE,
      input=c(readline("To change system setting requires superuser password. Please enter the password if you want to proceed: "), paste(new.blas.no))))
      if (engine == "OpenBLAS") message("R is linked with OpenBLAS")
      if (engine == "MKL") message("R is linked with Intel MKL")
  }
  message("Restart R session to activate the change.")
}


stop.mat.tool <- function(proceed=NULL) {
  if (is.null(proceed)) proceed <- readline("The function will change system-level settings. Do you want to proceed? (Yes/no/cancel)")
  if (!proceed %in% c("YES", "Y", "y", "yes", "Yes")) stop ("The function is stopped")
  currrent.blas.path <- file.path(extSoftVersion()["BLAS"])
  sys.name <- Sys.info()["sysname"]
  if (sys.name == "Linux") {
    linux.type <- system("grep ID_LIKE /etc/*-release", intern=TRUE)
    if (grepl("debian", linux.type)) sys.name <- "Debian"
  }
  if (sys.name != "Darwin" & sys.name != "Debian") stop ("The current version of the function only supports Mac OS and Debian Linux")
  if (! grepl("openblas", currrent.blas.path) & ! grepl("mkl", currrent.blas.path)) {
    stop("R is linked with the default BLAS. No need to stop.")
  }
  # For OS X
  if (sys.name == "Darwin") {
    lapack.lib <- La_library()
    rblas.lib <- gsub("lapack", "blas", lapack.lib)
    if (!file.exists(rblas.lib)) stop("Can't find the default Rblas")
    old.file <- paste(rblas.lib, "old", sep = ".")
    old.cmd <- paste('yes "y" | cp', currrent.blas.path, old.file, sep = " ")
    invisible(system(old.cmd, intern=TRUE))
    keep.file <- gsub("Rblas.dylib", "Rblas.dylib.keep", rblas.lib)
    if (!file.exists(keep.file)) {
    # Use backup file
    keep.file <- gsub("Rblas.dylib", "Rblas.dylib.backup", rblas.lib)
    }
    if (!file.exists(keep.file)) stop("Can't find the default Rblas")
    restore.cmd <- paste('yes "y" | mv', keep.file, rblas.lib, sep = " ")
    invisible(system(restore.cmd, intern=TRUE, ignore.stderr = TRUE))
    message("R is linked back with default BLAS")
    message("Restart R session to activate the change.")
  }
  # For Linux
  if (sys.name == "Debian") {
    blas.list <- system("update-alternatives --list libblas.so.3-x86_64-linux-gnu", intern = TRUE)
    for (i in 1:length(blas.list)) {
      if (grepl("/blas/libblas.so.3", blas.list[i])) blas.no <- i
    }
    restore.cmd1 <- paste("sudo -kS update-alternatives --config libblas.so.3-x86_64-linux-gnu")
    restore.cmd2 <- paste("sudo -kS update-alternatives --config liblapack.so.3-x86_64-linux-gnu")

    invisible(system(restore.cmd2, intern=TRUE, ignore.stderr = TRUE,
      input=c(readline("To change system setting requires superuser password. Please enter the password if you want to proceed: "), paste(blas.no))))
    invisible(system(restore.cmd1, intern=TRUE, ignore.stderr = TRUE,
      input=c(readline("To change system setting requires superuser password. Please enter the password if you want to proceed: "), paste(blas.no))))
    message("R is linked back with default BLAS")
    message("Restart R session to activate the change.")
  }
}
