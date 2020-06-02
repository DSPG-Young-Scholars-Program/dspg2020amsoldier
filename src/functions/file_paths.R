# set file path escape charcters given system type
file_path_escapes <- function(file_path, sys = Sys.info()[['sysname']]) {
  if (sys == "Linux" | sys == "Darwin")  return(gsub("([ \\(\\)])", "\\\\\\1", file_path))
  if (sys == "Windows") print("Seriously?")
}

