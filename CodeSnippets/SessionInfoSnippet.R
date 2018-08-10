# Session information to text file with date and time ####
writeLines(capture.output(sessionInfo()), 
           paste0(
             substring(timestamp(), 14, 22),
             ".", 
             substring(timestamp(), 24, 25),
             " - session.txt"))

