## This file is copied from GNATCOLL. Ideally, XML/Ada should
## depend on gnatcoll and let the latter compute once and for all
## whether the system supports shared libraries and whether the
## user wants them, but this is not the case currently.

#############################################################
# Check whether GNAT on that target supports building shared
# libraries
# The following variables is exported by configure:
#   @gnatmake@: the gnatmake command to use
#   @GNAT_BUILDS_SHARED@: either "yes" or "no"
#   @DEFAULT_LIBRARY_TYPE@: either "static" or "relocatable"
#       This is only set to "relocatable" if the user explicitly
#       added --enable-shared. Otherwise, even if we detect that
#       shared libs are available, we will still use static as the
#       safe default.
#############################################################

AC_DEFUN(AM_GNAT_BUILDS_SHARED,
[
   AC_MSG_CHECKING(whether gnat can build shared libs)

   DEFAULT_LIBRARY_TYPE=static

   AC_ARG_ENABLE(shared,
     [AC_HELP_STRING(
        [--disable-shared],
        [Disable building of shared libraries])
AC_HELP_STRING(
        [--enable-shared],
        [Build shared libraries if supported on the target
Make them the installation default])],
     [GNAT_BUILDS_SHARED=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [GNAT_BUILDS_SHARED=yes])

   if test "$program_prefix" != NONE; then
      gnatmake=${program_prefix}gnatmake
   else
      gnatmake=gnatmake
   fi

   AC_SUBST(gnatmake)

   if test x$GNAT_BUILDS_SHARED = xyes; then
      # Create a temporary directory (from "info autoconf")
      : ${TMPDIR=/tmp}
      {
        tmp=`(umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null` \
           && test -n "$tmp" && test -d "$tmp"
      } || {
        tmp=$TMPDIR/foo$$-$RANDOM
        (umask 077 && mkdir -p "$tmp")
      } || exit $?

      mkdir $tmp/lib
      echo "package Foo is end Foo;" > $tmp/foo.ads
      cat > $tmp/lib.gpr <<EOF
project Lib is
   for Source_Dirs use (".");
   for Library_Dir use "lib";
   for Library_Name use "lib";
   for Library_Kind use "relocatable";
end Lib;
EOF

      $gnatmake -c -q -P$tmp/lib 2>/dev/null
      if test $? = 0 ; then
         GNAT_BUILDS_SHARED=yes
      else
         GNAT_BUILDS_SHARED=no
      fi
      rm -rf $tmp
      AC_MSG_RESULT($GNAT_BUILDS_SHARED)
   else
      AC_MSG_RESULT([no (--disabled-shared)])
   fi

   AC_SUBST(GNAT_BUILDS_SHARED)
   AC_SUBST(DEFAULT_LIBRARY_TYPE)
])

