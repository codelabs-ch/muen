#
# Copyright (c) 2007-2013 Tero Koskinen <tero.koskinen@iki.fi>
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

PREFIX?=$(HOME)/libraries/ahven
INSTALL=install
OS_VERSION?=unix
LIBDIR?=$(PREFIX)/lib

SOURCES=src/ahven-framework.adb src/ahven-framework.ads \
	src/ahven-listeners-basic.adb src/ahven-listeners-basic.ads \
	src/ahven-listeners.ads \
	src/ahven-results.adb src/ahven-results.ads \
	src/ahven-runner.adb src/ahven-runner.ads \
	src/ahven-text_runner.adb src/ahven-text_runner.ads \
	src/ahven.adb src/ahven.ads \
	src/ahven-temporary_output.adb \
	src/ahven-temporary_output.ads \
	src/ahven-parameters.adb src/ahven-parameters.ads \
	src/ahven-xml_runner.adb src/ahven-xml_runner.ads \
	src/ahven-tap_runner.adb src/ahven-tap_runner.ads \
	src/ahven-astrings.ads \
	src/ahven-long_astrings.ads \
	src/${OS_VERSION}/ahven_compat.adb src/${OS_VERSION}/ahven_compat.ads \
	src/ahven-slist.adb src/ahven-slist.ads

ALI_FILES=lib/ahven.ali \
	lib/ahven_compat.ali \
	lib/ahven-framework.ali \
	lib/ahven-listeners-basic.ali \
	lib/ahven-listeners.ali \
	lib/ahven-results.ali \
	lib/ahven-runner.ali \
	lib/ahven-slist.ali \
	lib/ahven-tap_runner.ali \
	lib/ahven-parameters.ali \
	lib/ahven-temporary_output.ali \
	lib/ahven-text_runner.ali \
	lib/ahven-astrings.ali \
	lib/ahven-long_astrings.ali \
	lib/ahven-xml_runner.ali

STATIC_LIBRARY=libahven.a
GPR_FILE=gnat/ahven.gpr

default: build_all

objects:
	mkdir -p objects

test_objects:
	mkdir -p test_objects

lib:
	mkdir -p lib

build_all: objects test_objects build_lib build_tests

build_lib: objects lib
	OS_VERSION=$(OS_VERSION) gnatmake -Pgnat/ahven_lib

build_tests: test_objects build_lib
	OS_VERSION=$(OS_VERSION) gnatmake -Pgnat/ahven_tests

clean: clean_lib clean_tests clean_docs

clean_lib:
	gnatclean -q -Pgnat/ahven_lib

clean_tests:
	gnatclean -q -Pgnat/ahven_tests

clean_docs:
	rm -f doc/api/*.html ahven.specs

distclean:
	rm -rf lib objects results test_objects tester tap_tester

install: install_lib install_docs

install_lib: build_lib
	mkdir -p $(PREFIX)/include/ahven
	mkdir -p $(LIBDIR)/ahven
	mkdir -p $(PREFIX)/lib/gnat
	$(INSTALL) -m 644 $(SOURCES) $(PREFIX)/include/ahven
	$(INSTALL) -m 444 $(ALI_FILES) $(LIBDIR)/ahven
	$(INSTALL) -m 644 lib/$(STATIC_LIBRARY) $(LIBDIR)/ahven
	$(INSTALL) -m 644 $(GPR_FILE) $(PREFIX)/lib/gnat

install_docs: userguide
	mkdir -p $(PREFIX)/share/doc/ahven
	cp -r doc/manual/en/build/html $(PREFIX)/share/doc/ahven

check: build_tests
	./tester -c

check_xml: build_tests
	-mkdir -p results
	./tester -c -x -d results

check_tap: build_tests
	./tap_tester

control:
	rm -f objects/*.adt objects/*.ali
	cd objects && adactl -f ../rules/ahven.aru ../src/*.ad[bs] ../test/*.ad[bs] ../src/unix/*.ad[bs]
	rm -f objects/*.adt objects/*.ali

docs: userguide

old_docs: ahven.specs
	mkdir -p doc/api
	adabrowse -c adabrowse.conf -i -f@ahven.specs -o doc/api/

userguide:
	$(MAKE) -C doc/manual/en html

ahven.specs: $(SOURCES)
	find src/ -name "*.ads" -print |sort|uniq > ahven.specs

README.html: README.rst
	rst2html --stylesheet-path=css/html4css1.css,css/my-docutils.css README.rst > README.html

tags: $(SOURCES)
	ectags src/*.adb
