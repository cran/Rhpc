AC_INIT(src/common/Rhpc.h)

: ${R_HOME=`R RHOME`}

CHKLIBR=`${R_HOME}/bin${R_ARCH}/R CMD config --ldflags 2>/dev/null`
have_libR=no
if test -n "$CHKLIBR"; then
   have_libR=yes
fi
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
[   if test "${withval}x" != x; then
         MPICC=${withval}
    fi  ]	
)
AC_ARG_WITH(MPI_INCLUDE,
[   --with-mpi-cflags=-I/usr/include          Location of mpi.h directory.],
[   if test "${withval}x" != x; then
         MPI_CFLAGS=${withval}
    fi  ]	
)
AC_ARG_WITH(MPI_LIBS,
[   --with-mpi-ldflags=          MPI LIBS.],
[   if test "${withval}x" != x; then
         MPI_LDFLAGS=${withval}
    fi  ]	
)
AC_ARG_WITH(RHPC_PACKAGE_DIR,
[   --with-package-dir=          Top directory of Rhpc package],
[   if test "${withval}x" != x; then
        MY_PACKAGE_DIR=${withval}
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

if test -z "${MY_PACKAGE_DIR}" ; then
	if test "${RPM_BUILD_ROOT}x" != "x"; then
		MY_PACKAGE_DIR=${R_HOME}/library/Rhpc
	else
		MY_PACKAGE_DIR=${R_PACKAGE_DIR}
	fi
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


AC_SUBST(MPICC)
AC_SUBST(MPI_CFLAGS)
AC_SUBST(MPI_LDFLAGS)
AC_SUBST(LIBR)
AC_SUBST(R_HOME)
AC_SUBST(R_ARCH)
AC_SUBST(RCMDCONFIGLDFLAGS)
AC_SUBST(RCMDCONFIGCPPFLAGS)
AC_SUBST(MY_PACKAGE_DIR)
AC_SUBST(R_PACKAGE_DIR)

AC_OUTPUT(src/Makevars src/worker/Makefile src/worker/Rhpc src/worker/RhpcSpawn)

