#!/bin/sh
#
# Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#
# ./make_release <version>
#
# For example
# ./make_release 1.0
#

HGROOT=https://bitbucket.org/tkoskine/ahven

failure()
{
    echo "$1"
    exit 1
}

if [ x"$1" = x"" ]; then
    echo "usage: make_release <version>"
    exit 1
fi

VERSION=$1

cd /tmp || failure "cd /tmp failed"
hg clone $HGROOT ahven-$VERSION || failure "checkout failed"
cd ahven-$VERSION && rm -rf .hg .hgignore .hgtags && cd .. || failure "rm failed"
cd ahven-$VERSION && cd doc/manual/en && make html && cd /tmp || failure "docs failed"
tar zcf ahven-$VERSION.tar.gz ahven-$VERSION || failure "tar zcf failed"
zip -r ahven-$VERSION.zip ahven-$VERSION || failure "zip -r failed"

echo "Release tarball ready at /tmp/ahven-$VERSION.tar.gz"
echo "Release zip ready at /tmp/ahven-$VERSION.zip"
echo "Please remove /tmp/ahven-$VERSION directory."

echo
echo "Sign the tarball and the zip with commands"
echo "gpg --detach /tmp/ahven-$VERSION.tar.gz"
echo "gpg --detach /tmp/ahven-$VERSION.zip"

