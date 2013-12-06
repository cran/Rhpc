#include <sys/types.h>
#include <unistd.h>

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
static char RHPC_WORKER_CMD[]={R_PACKAGE_DIR "/RhpcSpawn"};
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
