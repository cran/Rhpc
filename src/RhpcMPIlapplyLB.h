/*
    Rhpc : R HPC environment
    Copyright (C) 2012-2015  Junji NAKANO and Ei-ji Nakama

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

__inline static int Rhpc_mpi_lapply_LB_active_workers(char *workers,int procs)
{
  int i;
  int cnt=0;
  DPRINT("procs=%d\n",procs);
  for (i=1;i<procs;i++){
    DPRINT("%d:",workers[i]);
    if(workers[i]!=0) cnt++;
  }
  DPRINT("\n");
  return(cnt);
}

__inline static void Rhpc_mpi_lapply_LB_exit(int procs, MPI_Comm comm)
{
  R_xlen_t i;
  int cmde[CMDLINESZ];

  MPI_Request *request=Calloc((procs-1),MPI_Request);
  MPI_Status  *status =Calloc((procs-1),MPI_Status);

  SET_CMD(cmde, CMD_NAME_LAPPLY_LB , SUBCMD_EXIT, 0, 0 );
  for (i=1; i < procs ; i++){
    DPRINT("worker exit rank=%ld\n",i);
    if(SYNC){
      _M(MPI_Send(cmde,   (int)CMDLINESZ, MPI_INT, i, RHPC_CTRL_TAG, comm));
    }else{
      _M(MPI_Isend(cmde,  (int)CMDLINESZ, MPI_INT, i, RHPC_CTRL_TAG, comm, &request[i-1]));
    }
  }
  if(!SYNC)
    _M(MPI_Waitall(procs-1, request, status));
  Free(request);
  Free(status);
}


static void Rhpc_mpi_lapply_LB_send(char *workers, int procs,
				 R_xlen_t *start, SEXP X,
				 MPI_Comm comm)
{
  R_xlen_t i,j;
  int errorOccurred=0;
  int cur_workers=0;
  R_xlen_t sendcalls=0;
  int cix;
  MPI_Request *request;
  MPI_Status  *status;

  R_xlen_t offset=*start;
  R_xlen_t xlen = xlength(X); 
  SEXP Xsel_l;
  SEXP Xsel;
  SEXP tag;
  SEXP sendlist;
  PROTECT_INDEX ix1;
  PROTECT_INDEX ix2;
  PROTECT_INDEX ix3;
  PROTECT_WITH_INDEX(Xsel_l    =R_NilValue, &ix1);
  PROTECT_WITH_INDEX(Xsel      =R_NilValue, &ix2);
  PROTECT_WITH_INDEX(tag       =R_NilValue, &ix3);
  PROTECT(sendlist=allocVector(VECSXP,procs-1));
  for (i=1; i < procs ; i++){
    SET_VECTOR_ELT(sendlist,i-1, R_NilValue);
  }

  DPRINT("SEND\n");
  
  for (cix=-1,i=1; i < procs ; i++){
    if((workers[i]==0) && offset+cur_workers < xlen ){
      cix ++;
      DPRINT("start=%ld:worker=%ld:cur_workers=%ld:pos=%ld\n",offset,i,cur_workers,cix+offset+1);
      workers[i]=1;
      cur_workers++;
      REPROTECT(tag = allocVector(VECSXP,2),ix3);
      SET_VECTOR_ELT(tag,0,ScalarReal((double)(cix+offset)));
      SET_VECTOR_ELT(tag,1,VECTOR_ELT(X,cix+offset));
      
      if(SERMODE){
	Xsel_l = LCONS(install("serialize"),CONS(tag,CONS(R_NilValue, CONS(ScalarLogical(FALSE), CONS(R_NilValue, CONS(R_NilValue, R_NilValue))))));
	Xsel_l = LCONS(install(".Internal"), CONS(Xsel_l, R_NilValue));
	REPROTECT(Xsel_l,ix1);
	REPROTECT(Xsel= R_tryEval(Xsel_l, R_GlobalEnv, &errorOccurred), ix2);
      }else{ /* SERMODE=0 */
	REPROTECT(Xsel= Rhpc_serialize(tag), ix2);
      }
      SET_VECTOR_ELT(sendlist,i-1, Xsel);
    }
  }
  
  /*calc send count;*/
  sendcalls=0;
  for (i=1; i < procs ; i++){
    if((workers[i]==1)){
      R_xlen_t lens = xlength(VECTOR_ELT(sendlist,i-1));
      R_xlen_t cnts = lens / RHPC_SPLIT_SIZE;
      R_xlen_t mods = lens % RHPC_SPLIT_SIZE;
      sendcalls+= 1 + cnts + ((mods)?1:0);
    }
  }
  
  request=Calloc(sendcalls,MPI_Request);
  status =Calloc(sendcalls,MPI_Status);
  
  if (sendcalls){
    sendcalls=0;
    for (i=1; i < procs ; i++){
      if((workers[i]==1)){
	int cmds[CMDLINESZ];
	R_xlen_t lens = xlength(VECTOR_ELT(sendlist,i-1));
	R_xlen_t cnts = lens / RHPC_SPLIT_SIZE;
	R_xlen_t mods = lens % RHPC_SPLIT_SIZE;
	SET_CMD(cmds, CMD_NAME_LAPPLY_LB , SUBCMD_NORMAL, cnts, mods );
	if(SYNC){
	  _M(MPI_Send(cmds,   (int)CMDLINESZ, MPI_INT, i,
		       RHPC_CTRL_TAG, comm));
	}else{
	  _M(MPI_Isend(cmds,   (int)CMDLINESZ, MPI_INT, i,
		       RHPC_CTRL_TAG, comm, &request[sendcalls]));
	}
	sendcalls++;
	/*
	  mydump((void*)cmds,CMDLINESZ*sizeof(int));
	*/
	for( j = 0 ; j< cnts ; j++){
	  if(SYNC){
	    _M(MPI_Send(RAW(VECTOR_ELT(sendlist,i-1))+RHPC_SPLIT_SIZE*j,
			 (int)RHPC_SPLIT_SIZE, MPI_CHAR,
			 (int)i, TAGCAL(j),    comm));
	  }else{
	    _M(MPI_Isend(RAW(VECTOR_ELT(sendlist,i-1))+RHPC_SPLIT_SIZE*j,
			 (int)RHPC_SPLIT_SIZE, MPI_CHAR,
			 (int)i, TAGCAL(j),    comm, &request[sendcalls]));
	  }
	  sendcalls++;
	}
	if ( mods != 0 ){
	  if(SYNC){
	    _M(MPI_Send(RAW(VECTOR_ELT(sendlist,i-1))+RHPC_SPLIT_SIZE*cnts,
			(int)mods,            MPI_CHAR,
			(int)i, TAGCAL(cnts), comm));
	  }else{
	    _M(MPI_Isend(RAW(VECTOR_ELT(sendlist,i-1))+RHPC_SPLIT_SIZE*cnts,
			 (int)mods,            MPI_CHAR,
			 (int)i, TAGCAL(cnts), comm, &request[sendcalls]));
	  }
	  sendcalls++;
	}
	workers[i]=2;
      }
    }
    if(!SYNC)
      _M(MPI_Waitall(sendcalls, request, status));
    Free(request);
    Free(status);
  }
  UNPROTECT(4);
  *start=*start+cur_workers;

}



SEXP Rhpc_mpi_lapply_LB(SEXP cl, SEXP X, SEXP args)
{
  R_xlen_t i;
  int errorOccurred=0;

  MPI_Comm comm;
  int procs;

  SEXP out, l_out=R_NilValue;

  R_xlen_t szi;
  R_xlen_t cnti;
  R_xlen_t modi;
  int cmd[CMDLINESZ];

  R_xlen_t xlen;
  R_xlen_t start;
  char *workers;
  SEXP          outlist_l;
  SEXP          outlist;
  SEXP          ans;
  PROTECT_INDEX outlist_l_ix;
  PROTECT_INDEX outlist_ix;
  PROTECT_INDEX ans_ix;
  SEXP indata;
  SEXP uns;
  SEXP uns_l;
  PROTECT_INDEX indata_ix;
  PROTECT_INDEX uns_l_ix;
  PROTECT_INDEX uns_ix;

  if(TYPEOF(cl)!=EXTPTRSXP){
    error("it's not MPI_Comm external pointer\n");
  }
  comm = SXP2COMM(cl);

  _M(MPI_Comm_size(comm, &procs));

  if(finalize){
    warning("Rhpc were already finalized.");
    return(R_NilValue);
  }
  if(!initialize){
    warning("Rhpc not initialized.");
    return(R_NilValue);
  }

  push_policy();

  /* -------- common start */
  /* serialize */
  if(SERMODE){
    l_out=LCONS(install("serialize"),CONS(args,CONS(R_NilValue, CONS(ScalarLogical(FALSE), CONS(R_NilValue, CONS(R_NilValue, R_NilValue))))));
    PROTECT(l_out=LCONS(install(".Internal"), CONS(l_out, R_NilValue)));
    PROTECT(out=R_tryEval(l_out, R_GlobalEnv, &errorOccurred));
  }else{ /* SERMODE=0 */
    PROTECT(l_out);
    PROTECT(out=Rhpc_serialize(args));
  }

  /* cmd send */
  szi = xlength(out);
  cnti = szi/RHPC_SPLIT_SIZE;
  modi = szi%RHPC_SPLIT_SIZE;
  SET_CMD(cmd, CMD_NAME_LAPPLY_LB,
	  SUBCMD_NORMAL,cnti, modi);
  _M(MPI_Bcast(cmd, CMDLINESZ, MPI_INT, 0, comm));

  /* send data */
  for(i = 0; i < cnti;i++){
    _M(MPI_Bcast(RAW(out)+ RHPC_SPLIT_SIZE*i,   (int)RHPC_SPLIT_SIZE, MPI_CHAR, 0, comm));

  }
  if( modi !=0 ){
    _M(MPI_Bcast(RAW(out)+ RHPC_SPLIT_SIZE*cnti, (int)modi,           MPI_CHAR, 0, comm));
  }
  /* -------- common end */

  xlen = xlength(X); 
  start =0;
  workers= Calloc(procs,char);

  PROTECT_WITH_INDEX(outlist_l=R_NilValue,               &outlist_l_ix);
  PROTECT_WITH_INDEX(outlist  =allocVector(VECSXP,xlen), &outlist_ix  );
  PROTECT_WITH_INDEX(ans      =R_NilValue,               &ans_ix);

  PROTECT_WITH_INDEX(indata   =R_NilValue,               &indata_ix);
  PROTECT_WITH_INDEX(uns_l    =R_NilValue,               &uns_l_ix);
  PROTECT_WITH_INDEX(uns      =R_NilValue,               &uns_ix);
  

  workers= Calloc(procs,char);
  memset((void*)workers,0,(procs));
  while(start<xlen || Rhpc_mpi_lapply_LB_active_workers(workers,procs)){
    MPI_Status stat;
    Rhpc_mpi_lapply_LB_send(workers, procs, &start, X, comm);

    if(Rhpc_mpi_lapply_LB_active_workers(workers,procs)){
      DPRINT("wait PROBE\n");
      _M(MPI_Probe(MPI_ANY_SOURCE, RHPC_CTRL_TAG, comm, &stat));
      DPRINT("recv from worker=%d\n",stat.MPI_SOURCE);
      if(stat.MPI_SOURCE != 0){
	int cmdr[CMDLINESZ];
	int wkr = stat.MPI_SOURCE;
	int  cmdmainr = 0;
	int  cmdsubr  = 0;
	R_xlen_t cntr = 0;
	R_xlen_t modr = 0;
	R_xlen_t lenr = 0;
	int msgcnt;
	int calls;
	R_xlen_t index;
	_M(MPI_Recv(cmdr, CMDLINESZ, MPI_INT, wkr, RHPC_CTRL_TAG, comm, &stat));
	GET_CMD(cmdr, &cmdmainr, &cmdsubr, &cntr, &modr);
	lenr = RHPC_SPLIT_SIZE * cntr + modr;
	REPROTECT(indata=allocVector(RAWSXP,lenr),indata_ix);
	
	msgcnt = cntr+((modr)?1:0);
	calls=0;
	{
	  MPI_Request *request = Calloc(msgcnt, MPI_Request);
	  MPI_Status  *status  = Calloc(msgcnt, MPI_Status);
	  for(i=0;i<cntr;i++){
	    if(SYNC){
	      _M(MPI_Recv(RAW(indata)+RHPC_SPLIT_SIZE*i, 
			   (int)RHPC_SPLIT_SIZE, MPI_CHAR,
			   wkr, TAGCAL(i),    comm, &status[calls]));
	    }else{
	      _M(MPI_Irecv(RAW(indata)+RHPC_SPLIT_SIZE*i, 
			   (int)RHPC_SPLIT_SIZE, MPI_CHAR,
			   wkr, TAGCAL(i),    comm, &request[calls]));
	    }
	    calls++;
	  }
	  if ( modr != 0 ){
	    if(SYNC){
	      _M(MPI_Recv(RAW(indata)+RHPC_SPLIT_SIZE*cntr,
			   (int)modr,            MPI_CHAR,
			   wkr, TAGCAL(cntr), comm, &status[calls]));
	    }else{
	      _M(MPI_Irecv(RAW(indata)+RHPC_SPLIT_SIZE*cntr,
			   (int)modr,            MPI_CHAR,
			   wkr, TAGCAL(cntr), comm, &request[calls]));
	    }
	    calls++;
	  }
	  if(!SYNC){
	    _M(MPI_Waitall(calls, request, status));
	  }
	  Free(status);
	  Free(request);
	}
	
	if(SERMODE){
	  uns_l=LCONS(install("unserialize"),CONS(indata, CONS(R_NilValue,R_NilValue)));
	  uns_l=LCONS(install(".Internal"),  CONS(uns_l,                  R_NilValue));
 
	  REPROTECT(uns_l, uns_l_ix);
	  REPROTECT(uns  = R_tryEval(uns_l, R_GlobalEnv, &errorOccurred),uns_ix);
	}else{/* SERMODE=0 */
	  REPROTECT(uns  = Rhpc_unserialize(indata), uns_ix);
	}
	index = (R_xlen_t)REAL(VECTOR_ELT(uns,0))[0];
	REPROTECT(ans = VECTOR_ELT(uns,1),ans_ix);
	SET_VECTOR_ELT(outlist, index, ans );
	workers[wkr]=0;
	DPRINT("finish rank=%d ix=%d\n", wkr, index);
      }
    }
  }
  Rhpc_mpi_lapply_LB_exit(procs, comm);
  UNPROTECT(8);
  Free(workers);

  pop_policy();

  return (_CHK(outlist));
}

