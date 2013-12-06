AC_INIT(src/common/Rhpc.h)

: ${R_HOME=`R RHOME`}

CHKLIBR=`${R_HOME}/bin${R_ARCH}/R CMD config --ldflags 2>/dev/null`
have_libR=no
if test -n "$CHKLIBR"; then
   have_libR=yes
fi
echo "###${have_libR}###${CHECKLIBR}###"
AC_MSG_CHECKING([libR check])
if test "${have_libR}" = no; then
	AC_MSG_RESULT(no)
	AC_MSG_ERROR([libR not found.
!!! Rhpc need R library. 
!!! Please add option --enable-R-shlib or
!!! --enable-R-static-lib rebuild R
])
else
	AC_MSG_RESULT(yes)
fi


AC_ARG_WITH(mpicc,
[   --with-mpicc=mpicc          Location of mpicc full path.],
[   if test "${withval}" != no; then
         MPICC=${withval}
    fi  ]	
)
AC_ARG_WITH(MPI_INCLUDE,
[   --with-mpi-cflags=-I/usr/include          Location of mpi.h directory.],
[   if test "${withval}" != no; then
         MPI_CFLAGS=${withval}
    fi  ]	
)
AC_ARG_WITH(MPI_LIBS,
[   --with-mpi-ldflags=          MPI LIBS.],
[   if test "${withval}" != no; then
         MPI_LDFLAGS=${withval}
    fi  ]	
)

if test -z "${MPICC}" ; then
	MPICC=mpicc
	RCC=`${R_HOME}/bin${R_ARCH}/R CMD config CC 2>/dev/null`
	argcnt=0
	for arg in $RCC; do
		if test $argcnt -eq 0 ; then
			argcnt=1
		else
			MPICC="${MPICC} ${arg}"
		fi
	done
	echo "configure: MPICC=$MPICC"
fi

#case "${host_os}" in
#  *-*-solaris*)
#	RPATH="-Wl,-R,"
#	;;
#  *)
#	RPATH="-Wl,-rpath,"
#esac



R_HOME=${R_HOME}
R_ARCH=${R_ARCH}
RCMDCONFIGLDFLAGS=$(${R_HOME}/bin/R CMD config --ldflags 2>/dev/null)
RCMDCONFIGCPPFLAGS=$(${R_HOME}/bin/R CMD config --cppflags 2>/dev/null)


YEARSTAMP=$(date '+%y')
YDAY=$(date '+%j')
YDAYSTAMP=$(expr ${YDAY} + 1000|sed -e 's/^.//')
HOUR=$(date '+%H')
HOURSTAMP=$(expr ${HOUR} + 100 |sed -e 's/^.//')
DATESTAMP=$(date '+%Y-%m-%d')


AC_SUBST(YEARSTAMP)
AC_SUBST(YDAYSTAMP)
AC_SUBST(HOURSTAMP)
AC_SUBST(DATESTAMP)
AC_SUBST(MPICC)
AC_SUBST(MPI_CFLAGS)
AC_SUBST(MPI_LDFLAGS)
AC_SUBST(LIBR)
AC_SUBST(R_HOME)
AC_SUBST(R_ARCH)
AC_SUBST(RCMDCONFIGLDFLAGS)
AC_SUBST(RCMDCONFIGCPPFLAGS)
AC_SUBST(R_PACKAGE_DIR)

AC_OUTPUT(src/dust/DESCRIPTION src/Makevars src/worker/Makefile src/worker/Rhpc src/worker/RhpcSpawn)
mv src/dust/DESCRIPTION DESCRIPTION

