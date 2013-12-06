Rhpc_EvalQ<-function(cl, expr, envir=.GlobalEnv)
    Rhpc_worker_call(cl, eval, substitute(expr), envir=envir)

