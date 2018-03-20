# print a message into konsole given the message string for logging purposes
printLog <- function(msg=NULL, init=F, finit=F){
  # if(!PRINT_LOGS) return()

  if(init){
    message(paste('\n--------------------------------------------------------------------\n',
                  as.character(Sys.time()),'New session just started!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }

  if(finit){
    message(paste('\n--------------------------------------------------------------------\n',
                  as.character(Sys.time()),'Initial setup was completed!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }

  message(paste(as.character(Sys.time()), msg, '\t'))
}
