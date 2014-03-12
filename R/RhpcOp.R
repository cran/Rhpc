Rhpc_enquote<- function(...)
{
  args <- list(...)
  .Call("Rhpc_enquote", args, PACKAGE="Rhpc")
}

Rhpc_splitList <- function(var,num)
{
  .Call("Rhpc_splitList", as.list(var),as.integer(num), PACKAGE="Rhpc")
}

Rhpc_mode <- function(mode = 0)
{
  if ( typeof(mode) %in% c("double","integer") )
    .Call("Rhpc_mode", as.integer(mode), PACKAGE="Rhpc")
  else
    .Call("Rhpc_mode", NULL, PACKAGE="Rhpc")
}

Rhpc_serialize_mode <- function(mode = 0)
{
  if ( typeof(mode) %in% c("double","integer") )
    .Call("Rhpc_serialize_mode", as.integer(mode), PACKAGE="Rhpc")
  else
    .Call("Rhpc_serialize_mode", NULL, PACKAGE="Rhpc")
}

