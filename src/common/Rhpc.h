
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



#if 0
struct timeval ts,te;
#define GETTIME(x) gettimeofday(&x, NULL);
#define TMPRINT(fmt) Rprintf(fmt, (double)(te.tv_sec - ts.tv_sec) + ((double)te.
#define DPRINT(...) Rprintf(__VA_ARGS__)
#else
#define GETTIME(x)
#define TMPRINT(fmt)
__inline static void DPRINT(char *fmt,...)
{
  va_list arg;  
  if(0){
    va_start(arg, fmt);
    vprintf(fmt, arg);
    va_end(arg);
  }
  return;
}
#endif

__inline static int _M(int _X)
{
  int  _ec = _X;
  int  _el = 0;
  char _em[MPI_MAX_ERROR_STRING];
  if ( MPI_SUCCESS != _ec ){
    MPI_Error_string(_ec, _em, &_el);
    error(_em);
  }
  return _ec;
}

__inline static SEXP _CHK(SEXP _X)
{
  SEXP ret = _X;
  R_xlen_t errcnt=0;
  R_xlen_t i;
  char msg[512]="";
  char call[512]="";
  PROTECT(_X);
  for(i=0;i<xlength(_X);i++){
    SEXP target;
    SEXP klass;
    PROTECT(target = VECTOR_ELT(_X,i));
    PROTECT(klass = getAttrib(target, R_ClassSymbol));
    if(xlength(klass)==1 && 0==strcmp(CHAR(STRING_ELT(klass, 0)),"Rhpc-try-error")){
      if(errcnt==0){
	strncpy(msg,CHAR(STRING_ELT(VECTOR_ELT(target,0),0)),sizeof(msg)-1);
	msg[sizeof(msg)-1]=0;
	call[sizeof(call)-1]=0;
      }
      errcnt++;
    }
    UNPROTECT(2);
  }
  if(errcnt==1){
    error( "one remote produced an error: %s\n%s\n", msg, call);
    ret = R_UnboundValue;
  }else if (errcnt>1){
    error( "%d remote produced an errors; first error: %s\n%s\n", errcnt, msg, call);
    ret = R_UnboundValue;
  }
  UNPROTECT(1);
  return ret;
}


SEXP Rhpc_serialize(SEXP);
SEXP Rhpc_unserialize(SEXP);

#if !defined(WORKER) /* master only */
static char RHPC_WORKER_CMD[]={MY_PACKAGE_DIR "/RhpcSpawn"};
#endif
#define RHPC_SPLIT_SIZE (1UL<<24)
#define CMDLINESZ 4
#define RHPC_CTRL_TAG 0
#define TAGCAL(_x) ((int)_x + 1)

enum CMD_NAME{
  CMD_NAME_UNKNOWN,
  CMD_NAME_WORKERCALL_NORET,
  CMD_NAME_WORKERCALL_RET,
  CMD_NAME_WORKERCALL_EXPORT,
  CMD_NAME_LAPPLY_LB,
  CMD_NAME_LAPPLY_SEQ,
  CMD_NAME_MODE,
  CMD_NAME_SERIALIZE_MODE,
  CMD_NAME_ENDL
};

enum SUBCMD_NAME{
  SUBCMD_NORMAL,
  SUBCMD_EXIT
};

enum POS_NAME{
  CMD_MAIN,
  CMD_SUB,
  CMD_CNT,
  CMD_MOD
};

__inline static void SET_CMD(int *cmd, int m, int s, R_xlen_t cnt, R_xlen_t mod)
{
  cmd[CMD_MAIN]=m;
  cmd[CMD_SUB]=s;
  cmd[CMD_CNT]=(int)cnt;
  cmd[CMD_MOD]=(int)mod;
}

__inline static void GET_CMD(int *cmd, int *m, int *s, R_xlen_t *cnt, R_xlen_t *mod)
{
  *m=cmd[CMD_MAIN];
  *s=cmd[CMD_SUB];
  *cnt=(R_xlen_t)cmd[CMD_CNT];
  *mod=(R_xlen_t)cmd[CMD_MOD];
  DPRINT("CMD[%d]:%d:%d:%d:%d\n", getpid(),cmd[0], cmd[1], cmd[2], cmd[3]); 
} 

#if defined(WORKER)
static char *MPI_argv[]={"RhpcWorker",
			 "--gui none",
			 "--silent",
			 "--no-restore",
			 "--no-save",
			 "--no-readline"};
static int MPI_argc = sizeof(MPI_argv)/sizeof(char*);
#else
static char *MPI_argv[1]={"R"};
static int MPI_argc = 1;
#endif

#ifndef MYSCHED
int MYSCHED;
#endif
__inline void push_policy(void)
{
#if (defined(_POSIX_PRIORITY_SCHEDULING) && defined(HAVE_SCHED_GETSCHEDULER))
  struct sched_param sp;
  int policy_max;
  MYSCHED = sched_getscheduler(0);
  if(-1 == (policy_max = sched_get_priority_max(SCHED_FIFO))){
    policy_max=0;
  }
  sp.sched_priority = policy_max;
  sched_setscheduler(0, SCHED_FIFO, &sp);
  /* Rprintf("SCHED=%d:max=%d\n", SCHED_FIFO, sp.sched_priority); */
#else
  MYSCHED = 1;
#endif /* _POSIX_PRIORITY_SCHEDULING */
}
__inline void pop_policy(void)
{
#if (defined(_POSIX_PRIORITY_SCHEDULING) && defined(HAVE_SCHED_GETSCHEDULER))
  struct sched_param sp;
  int policy_max;
  if(-1 == (policy_max = sched_get_priority_max(MYSCHED))){
    policy_max=0;
  }
  sp.sched_priority = policy_max;
  sched_setscheduler(0, MYSCHED, &sp);
  /* Rprintf("SCHED=%d:max=%d\n", MYSCHED, sp.sched_priority); */
#else
  MYSCHED=0;
#endif /* _POSIX_PRIORITY_SCHEDULING */
}

#include <R_ext/Parse.h>
static void op_comm_free(SEXP com)
{
  void *ptr;
  if (TYPEOF(com) != EXTPTRSXP)
    error("not external pointer");
  ptr = R_ExternalPtrAddr(com);
  Free(ptr);
}

__inline static void Rhpc_set_options(int rank, int procs, MPI_Comm ccomm)
{
  SEXP op_ex;
  SEXP op_nm;
  MPI_Comm *ptr;
  SEXP com=R_NilValue;
  int errorOccurred=0;

  if(rank != -1){
    ptr = Calloc(1,MPI_Comm);
    PROTECT(com = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
    *((MPI_Comm*)R_ExternalPtrAddr(com)) = ccomm;
    R_RegisterCFinalizer(com, op_comm_free);
    PROTECT(op_ex = LCONS(install("options"),
			  CONS(ScalarInteger(rank),
			       CONS(ScalarInteger(procs),
				    CONS(com,
					 CONS(ScalarInteger(MPI_Comm_c2f(ccomm)),
					      R_NilValue))))));
  }else{
    PROTECT(com);
    PROTECT(op_ex = LCONS(install("options"),
			  CONS(R_NilValue,
			       CONS(R_NilValue,
				    CONS(R_NilValue,
					 CONS(R_NilValue,
					      R_NilValue))))));
  }
  PROTECT(op_nm = allocVector(STRSXP, 5));
  SET_STRING_ELT(op_nm,0,mkChar(""));
  SET_STRING_ELT(op_nm,1,mkChar("Rhpc.mpi.rank"));
  SET_STRING_ELT(op_nm,2,mkChar("Rhpc.mpi.procs"));
  SET_STRING_ELT(op_nm,3,mkChar("Rhpc.mpi.c.comm"));
  SET_STRING_ELT(op_nm,4,mkChar("Rhpc.mpi.f.comm"));
  setAttrib(op_ex, R_NamesSymbol, op_nm);
  R_tryEval(op_ex, R_GlobalEnv, &errorOccurred);

  UNPROTECT(3);
}



#ifdef DEBUG
static void mydump(unsigned char* p, size_t sz)
{
  size_t i,j;
  char obuf[512];
  char buf[512];
  obuf[0]=0;
  printf("%d:%d\n",getpid(),(int)sz);
  for ( i = 0; i<sz;i+=16){
    buf[0]=0;
    for( j=0; j<16&& i+j<sz; j++){
      sprintf(buf+strlen(buf),"%02x ", p[i+j]);
    }
    sprintf(buf+strlen(buf),"\n");
    if(strcmp(buf,obuf)!=0){
      printf ("%d:%05lx: ", getpid(), i);
      printf (buf);
      strcpy(obuf,buf);
    }
  }
}

static void STR(SEXP _x)
{
  SEXP  _l,_f;
  int errorOccurred=0;
  _l = LCONS(install("str"),CONS(_x, R_NilValue));
  _l = LCONS(install("try"),CONS(_l, R_NilValue));
  PROTECT(_l);
  PROTECT(_f=R_tryEval(_l, R_GlobalEnv, &errorOccurred));
  UNPROTECT(2);
}

#endif
