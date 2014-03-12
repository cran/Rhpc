\name{Rhpc-package}
\alias{Rhpc-package}
\alias{Rhpc_initialize}
\alias{Rhpc_finalize}
\alias{Rhpc_getHandle}
\alias{Rhpc_worker_call}
\alias{Rhpc_worker_shy}
\alias{Rhpc_lapply}
\alias{Rhpc_lapplyLB}
\alias{Rhpc_numberOfWorker}
\alias{Rhpc_Export}
\alias{Rhpc_EvalQ}
\alias{Rhpc_setupRNG}
\alias{Rhpc_mode}
\alias{Rhpc_serialize_mode}
\alias{Rhpc_enquote}
\alias{Rhpc_splitList}
\alias{Rhpc_serialize}
\alias{Rhpc_unserialize}

\usage{
Rhpc_initialize()
Rhpc_finalize()
Rhpc_getHandle(procs)
Rhpc_worker_call(cl, FUN, ...)
Rhpc_worker_shy(cl, FUN, ...)
Rhpc_lapply(cl, X, FUN, ...)
Rhpc_lapplyLB(cl, X, FUN, ...)
Rhpc_numberOfWorker(cl)
Rhpc_Export(cl, variableNames, pos, envir)
Rhpc_EvalQ(cl, expr, envir)
Rhpc_setupRNG(cl, iseed)
}
\arguments{
  \item{cl}{external pointer to MPI communicator}
  \item{procs}{number of process, void if Rhpc}
  \item{X}{vector or list}
  \item{FUN}{string of function name or function object}
  \item{...}{arguments}
  \item{variableNames}{string vector} 
  \item{iseed}{random number seed}
  \item{expr}{expression to evaluate}
  \item{pos}{default is current environment position}
  \item{envir}{where to look for the object}
}
\docType{package}
\title{
  A package for High-Performance Computing
}
\description{
 Rhpc_lapply, Rhpc_lapplyLB and Rhpc_worker_call using MPI provides 
 better HPC environment on R(works fast on HPC).
 Maybe start from the Rhpc batch command is better(faster).
 Because strong mutual concessions(slowly) often occur by sched_yield, we recommend the use with the batch mode...XD

 Rhpc batch in package directory.
}
\details{
  \url{http://prs.ism.ac.jp/~nakama/Rhpc}
}
\author{
  Junji Nakano <nakanoj@ism.ac.jp> and Ei-ji Nakama<nakama@com-one.com>
 
  Maintainer: Ei-ji Nakama <nakama@com-one.com>
}
\keyword{utilities}
\examples{
\dontrun{
# use Rhpc command
Rhpc_initialize()
cl <- Rhpc_getHandle() # ommit number of worker if use Rhpc batch

# Rhpc set to options
opstr=list("Rhpc.mpi.rank","Rhpc.mpi.procs","Rhpc.mpi.c.comm","Rhpc.mpi.f.comm")
do.call("options",opstr)
Rhpc_worker_call(cl, "do.call","options", opstr)
# warning! : pointer not export, worker Rhpc.mpi.c.comm is (nil) on master.

Rhpc_worker_call(cl, Sys.getpid)
Rhpc_lapply(cl,1:10000,sqrt)
Rhpc_lapplyLB(cl,1:10000,sqrt)
Rhpc_finalize()
#
# command line example
#
# mpirun -n 4 Rhpc CMD BATCH --no-save `script file for exemplified above'
#
}
}
