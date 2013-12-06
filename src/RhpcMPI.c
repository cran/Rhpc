/*
    Rhpc : R HPC environment
    Copyright (C) 2012-2013  Junji Nakano and Ei-ji Nakama

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License,
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#pragma GCC diagnostic ignored "-Wunused-parameter"
#include <mpi.h>
#pragma GCC diagnostic warning "-Wunused-parameter"
#include <dlfcn.h>
#include <sched.h>


#include <R.h>
#include <Rinternals.h>
#include "common/Rhpc.h"

static int initialize=0;
static int finalize=0;

static int MPI_rank = 0;
static int MPI_procs = 0;

static MPI_Comm RHPC_Comm = MPI_COMM_NULL;

static int SYNC = 0;
static int SERMODE=0;


SEXP Rhpc_mpi_initialize(void)
{
  int *mpi_argc = (int *)&MPI_argc;
  char ***mpi_argv= (char ***)MPI_argv;

  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(initialize){
    warning("Rhpc were already initialized.");
    return(R_NilValue);
  }


#ifdef OPEN_MPI
#ifndef __APPLE__
  if (!dlopen("libmpi.so.0", RTLD_GLOBAL | RTLD_LAZY) && 
      !dlopen("libmpi.so",   RTLD_GLOBAL | RTLD_LAZY)    ){
    Rprintf("%s\n",dlerror());
  }
#endif
#endif
#if defined(MPI_VERSION) && (MPI_VERSION >= 2)
  mpi_argc=NULL;
  mpi_argv=NULL;
#endif
  _M(MPI_Init(mpi_argc, mpi_argv));
  _M(MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN));
  _M(MPI_Errhandler_set(MPI_COMM_SELF, MPI_ERRORS_RETURN));
  _M(MPI_Comm_rank(MPI_COMM_WORLD, &MPI_rank));
  _M(MPI_Comm_size(MPI_COMM_WORLD, &MPI_procs));

  RHPC_Comm = MPI_COMM_WORLD;

  DPRINT("Rhpc_initialize : rank:%d size:%d\n", MPI_rank, MPI_procs);
  initialize = 1;
  return(R_NilValue);
}

static void comm_free(SEXP com)
{
  void *ptr;
  if (TYPEOF(com) != EXTPTRSXP)
    error("not external pointer");
  ptr = R_ExternalPtrAddr(com);
  Free(ptr);
}

#define SXP2COMM(x) *((MPI_Comm*)R_ExternalPtrAddr(x))
#define SXP2COMMP(x) ((MPI_Comm*)R_ExternalPtrAddr(x))

SEXP Rhpc_gethandle(SEXP procs)
{
  int num_procs;
  MPI_Comm *ptr;
  SEXP com;
  int num;
  MPI_Comm pcomm;

  if (RHPC_Comm == MPI_COMM_NULL){
    error("Rhpc_initialize is not called.");
    return(R_NilValue);
  }
  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(!initialize){
    warning("Rhpc not initialized.");
    return(R_NilValue);
  }
 
  num_procs = INTEGER (procs)[0];

  ptr = Calloc(1,MPI_Comm);
  PROTECT(com = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(com, comm_free);
  SXP2COMM(com) = RHPC_Comm;

  if (getenv("USE_RHPC")){/* use mpirun */
    _M(MPI_Comm_size(SXP2COMM(com), &num));
    Rprintf("Detected communication size %d\n", num);
    if( num > 1 ){
      if ( num_procs > 0){
	warning("blind procs argument, return of MPI_COMM_WORLD");
      }
    }else{
      SXP2COMM(com)=MPI_COMM_NULL;
      warning("require mpirun or mpiexec with shell for Rhpc");
    }
    UNPROTECT(1);
    return(com);
  }else{ /* spawn */
    if(num_procs < 1){
      warning("you need positive number of procs argument");
      UNPROTECT(1);
      return(com);
    }
    _M(MPI_Comm_size(SXP2COMM(com), &num));
    if(num > 1){ 
      warning("blind procs argument, return of last communicator");
      UNPROTECT(1);
      return(com);
    }
  }

  _M(MPI_Comm_spawn(RHPC_WORKER_CMD, MPI_ARGV_NULL, num_procs,
		    MPI_INFO_NULL, 0, MPI_COMM_SELF, &pcomm,  
		    MPI_ERRCODES_IGNORE));
  _M(MPI_Intercomm_merge( pcomm, 0, SXP2COMMP(com)));
  _M(MPI_Comm_free( &pcomm ));
  _M(MPI_Comm_size(SXP2COMM(com), &num));
  RHPC_Comm = SXP2COMM(com); /* rewrite RHPC_Comm */
  _M(MPI_Errhandler_set(RHPC_Comm, MPI_ERRORS_RETURN));
  UNPROTECT(1);
  return(com);
}

SEXP Rhpc_mpi_finalize(void)
{
  int cmd[CMDLINESZ];

  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(!initialize){
    warning("Rhpc not initialized.");
    return(R_NilValue);
  }

  SET_CMD(cmd, CMD_NAME_ENDL, SUBCMD_NORMAL, 0, 0);
  _M(MPI_Bcast(cmd, CMDLINESZ, MPI_INT, 0, RHPC_Comm));
  _M(MPI_Finalize());
  finalize =1;
  return(R_NilValue);
}

SEXP Rhpc_mode(SEXP mode)
{
  SEXP res;
  int cmd[CMDLINESZ];

  PROTECT(res=allocVector(INTSXP,1));
  if ( TYPEOF(mode) == INTSXP && xlength(mode)>=1 ){
    SYNC=INTEGER(mode)[0];
    if (SYNC != 0 && SYNC != 1) SYNC=0;
    INTEGER(res)[0]=SYNC;
    SET_CMD(cmd, CMD_NAME_MODE, SYNC, 0, 0);
    _M(MPI_Bcast(cmd, CMDLINESZ, MPI_INT, 0, RHPC_Comm));
  }else{
    INTEGER(res)[0]=SYNC;
  }
  UNPROTECT(1);
  return(res);
}

SEXP Rhpc_serialize_mode(SEXP mode)
{
  SEXP res;
  int cmd[CMDLINESZ];

  PROTECT(res=allocVector(INTSXP,1));
  if ( TYPEOF(mode) == INTSXP && xlength(mode)>=1 ){
    SERMODE=INTEGER(mode)[0];
    if (SERMODE != 0 && SERMODE != 1) SERMODE=0;
    INTEGER(res)[0]=SERMODE;
    SET_CMD(cmd, CMD_NAME_SERIALIZE_MODE, SERMODE, 0, 0);
    _M(MPI_Bcast(cmd, CMDLINESZ, MPI_INT, 0, RHPC_Comm));
  }else{
    INTEGER(res)[0]=SERMODE;
  }
  UNPROTECT(1);
  return(res);
}

SEXP Rhpc_number_of_worker(SEXP cl)
{
  MPI_Comm comm = SXP2COMM(cl);
  SEXP num;
  int procs;

  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(!initialize){
    warning("Rhpc not initialized.");
    return(R_NilValue);
  }

  PROTECT(num=allocVector(INTSXP,1));
  _M(MPI_Comm_size(comm, &procs));
  INTEGER(num)[0]=procs-1;
  UNPROTECT(1);
  return(num);
}


SEXP Rhpc_get_mpi_rank(SEXP cl)
{
  MPI_Comm comm = SXP2COMM(cl);
  SEXP num;
  int rank;

  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(!initialize){
    warning("Rhpc not initialized.");
    return(R_NilValue);
  }

  PROTECT(num=allocVector(INTSXP,1));
  _M(MPI_Comm_rank(comm, &rank));
  INTEGER(num)[0]=rank;
  UNPROTECT(1);
  return(num);
}

#include "common/Rhpc_ms.h"
#include "RhpcMPIlapplyLB.h"
#include "RhpcMPIlapplyseq.h"
#include "RhpcMPIWorkerCall.h"

