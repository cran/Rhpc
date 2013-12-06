SEXP Rhpc_enquote(SEXP arg)
{
  R_xlen_t i;
  SEXP argq=R_NilValue;
  SEXP nm=getAttrib(arg, R_NamesSymbol);
  PROTECT_INDEX pqix;
  PROTECT(nm);
  PROTECT_WITH_INDEX(argq, &pqix);  
  argq = allocVector(VECSXP, xlength(arg));
  REPROTECT(argq , pqix);
  for(i=0;i<xlength(arg);i++){
    SEXP ll =  LCONS(install("quote"),CONS(VECTOR_ELT(arg,i),R_NilValue));
    SET_VECTOR_ELT(argq, i, ll);
    REPROTECT(argq ,pqix);
  }
  setAttrib(argq, R_NamesSymbol, duplicate(nm));
  REPROTECT(argq ,pqix);
  UNPROTECT(2);
  return(argq);
}


#define SPLITSIZEIX(LEN,SPLIT,IX) (LEN/SPLIT+((IX<(LEN%SPLIT))?1:0)) 

SEXP Rhpc_splitList(SEXP orgList, SEXP splitNum)
{
  R_xlen_t spnum;
  R_xlen_t sz;
  SEXP outList;
  SEXP origListNm;
  PROTECT_INDEX outList_ix;
  R_xlen_t i;
  R_xlen_t j;

  if(TYPEOF(orgList) != VECSXP ) return (orgList);

  spnum = INTEGER(splitNum)[0];
  sz = xlength(orgList);

  outList = allocVector(VECSXP,spnum);
  PROTECT_WITH_INDEX(outList, &outList_ix);

  PROTECT(origListNm = getAttrib(orgList, R_NamesSymbol));

  for ( i=0; i< spnum; i++){
    SEXP work;
    SEXP workNm;
    PROTECT(work   = allocVector(VECSXP,SPLITSIZEIX(sz,spnum,i)));
    PROTECT(workNm = allocVector(STRSXP,SPLITSIZEIX(sz,spnum,i)));
    for ( j=i ; j<sz; j+=spnum ){
      R_xlen_t k = j / spnum;
      SET_VECTOR_ELT(work,   k, VECTOR_ELT(orgList, j));
      if(origListNm != R_NilValue)
	SET_STRING_ELT(workNm, k, STRING_ELT(origListNm, j));
    }
    if(origListNm != R_NilValue)
      setAttrib(work, R_NamesSymbol, workNm);
    SET_VECTOR_ELT(outList, i, work);
    REPROTECT(outList, outList_ix);
    UNPROTECT(2);    
  }
  UNPROTECT(2);
  return(outList);
}


