#!/bin/sh

fail()
{
	echo $*
	exit 1
}

if [ x"$1" = x"" ]; then
    echo "usage: test_release.sh <version>"
    exit 1
fi

VERSION=$1

TEMPDIR=`mktemp -d`
INSTALL_DIR=`mktemp -d`

cd $TEMPDIR || fail "cd to temp failed"
tar zxvf /tmp/ahven-$VERSION.tar.gz || fail "tar failed"
cd ahven-$VERSION || fail "cd to ahven-$VERSION failed"
make || fail "make failed"
make check || fail "make check failed"
make check_xml || fail "make check_xml failed"
make check_tap || fail "make check_tap failed"
make docs || fail "make docs failed"
make control || fail "make control failed"
make PREFIX=$INSTALL_DIR install || fail "make install failed"
cd examples || fail "cd examples failed"
export ADA_PROJECT_PATH=$INSTALL_DIR/lib/gnat
gnatmake -Pexamples || fail "gnatmake -Pexamples failed"

echo "EVERYTHING OK"
echo
echo "Please remove directories $TEMPDIR and $INSTALL_DIR"
echo "  rm -rf $TEMPDIR $INSTALL_DIR"
echo


