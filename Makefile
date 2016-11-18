# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.

###############

# Hello! You're looking at a fork of flow. Most of these makefile build steps
# are from Flow (would be nice to trim this down; we only need the flow parser
# part). The only step we care about is `build-flow`, below.

###############

OS=$(shell uname -s)

FLOWLIB=bin/flowlib.tar.gz

ifeq ($(OS), Linux)
  INOTIFY=third-party/inotify
  INOTIFY_STUBS=$(INOTIFY)/inotify_stubs.c
  FSNOTIFY=fsnotify_linux
  ELF=elf
  RT=rt
  FRAMEWORKS=
  SECTCREATE=
endif
ifeq ($(OS), Darwin)
  INOTIFY=fsevents
  INOTIFY_STUBS=$(INOTIFY)/fsevents_stubs.c
  FSNOTIFY=fsnotify_darwin
  ELF=
  RT=
  FRAMEWORKS=CoreServices CoreFoundation
  SECTCREATE=-cclib -sectcreate -cclib __text -cclib flowlib -cclib $(abspath $(FLOWLIB))
endif

################################################################################
#                                 Definitions                                  #
################################################################################

MODULES=\
  src/commands\
  src/common\
  src/dts\
  src/embedded\
  src/parser\
  src/parsing\
  src/server\
  src/services/autocomplete\
  src/services/inference\
  src/services/flowFileGen\
  src/services/port\
  src/stubs\
  src/typing\
  hack/dfind\
  hack/find\
  hack/globals\
  hack/heap\
  hack/hhi\
  hack/injection/default_injector\
  hack/procs\
  hack/search\
  hack/socket\
  hack/stubs\
  hack/third-party/avl\
  hack/third-party/core\
  hack/utils\
  hack/utils/collections\
  hack/utils/disk\
  hack/utils/hh_json\
  hack/$(INOTIFY)\
  hack/$(FSNOTIFY)

NATIVE_C_FILES=\
  hack/$(INOTIFY_STUBS)\
  hack/heap/hh_shared.c\
  hack/hhi/hhi_elf.c\
  hack/utils/files.c\
  hack/utils/get_build_id.c\
  hack/utils/handle_stubs.c\
  hack/utils/nproc.c\
  hack/utils/realpath.c\
  hack/utils/sysinfo.c\
  hack/utils/priorities.c\
  hack/utils/win32_support.c\
  hack/hhi/hhi_win32res_stubs.c\
  src/embedded/flowlib_elf.c

OCAML_LIBRARIES=\
  unix\
  str\
  bigarray

NATIVE_LIBRARIES=\
  pthread\
  $(ELF)\
  $(RT)

OCP_BUILD_FILES=\
  ocp_build_flow.ocp\
  ocp_build_hack.ocp

FILES_TO_COPY=\
  $(wildcard lib/*.js)

JS_STUBS=\
	$(wildcard js/*.js)

# We need caml_hexstring_of_float for js_of_ocaml < 2.8
# JSOO_VERSION=$(shell which js_of_ocaml 2> /dev/null > /dev/null && js_of_ocaml --version)
# JSOO_MAJOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 1)
# JSOO_MINOR=$(shell echo $(JSOO_VERSION) | cut -d. -f 2)
# ifeq (1, $(shell [ $(JSOO_MAJOR) -gt 2 ] || [ $(JSOO_MAJOR) -eq 2 -a $(JSOO_MINOR) -gt 7 ]; echo $$?))
# 	JS_STUBS += js/optional/caml_hexstring_of_float.js
# endif

################################################################################
#                                    Rules                                     #
################################################################################

ALL_HEADER_FILES=$(addprefix _build/,$(shell find hack -name '*.h'))
NATIVE_OBJECT_FILES=$(patsubst %.c,%.o,$(NATIVE_C_FILES))
NATIVE_OBJECT_FILES+=hack/utils/get_build_id.gen.o
BUILT_C_FILES=$(addprefix _build/,$(NATIVE_C_FILES))
BUILT_OBJECT_FILES=$(addprefix _build/,$(NATIVE_OBJECT_FILES))

CC_FLAGS=
CC_FLAGS += $(EXTRA_CC_FLAGS)
CC_OPTS=$(foreach flag, $(CC_FLAGS), -ccopt $(flag))
INCLUDE_OPTS=$(foreach dir,$(MODULES),-I $(dir))
LIB_OPTS=$(foreach lib,$(OCAML_LIBRARIES),-lib $(lib))
NATIVE_LIB_OPTS=$(foreach lib, $(NATIVE_LIBRARIES),-cclib -l -cclib $(lib))
EXTRA_INCLUDE_OPTS=$(foreach dir, $(EXTRA_INCLUDE_PATHS),-ccopt -I -ccopt $(dir))
EXTRA_LIB_OPTS=$(foreach dir, $(EXTRA_LIB_PATHS),-cclib -L -cclib $(dir))
FRAMEWORK_OPTS=$(foreach framework, $(FRAMEWORKS),-cclib -framework -cclib $(framework))

BYTECODE_LINKER_FLAGS=$(NATIVE_OBJECT_FILES) $(NATIVE_LIB_OPTS) $(EXTRA_LIB_OPTS) $(FRAMEWORK_OPTS)
LINKER_FLAGS=$(BYTECODE_LINKER_FLAGS) $(SECTCREATE)


all: build-flow

clean:
	rm -rf _build
	rm -rf bin
	rm -f hack/utils/get_build_id.gen.c

# This is the part where compile our single file, src/main.re
build-flow: $(BUILT_OBJECT_FILES) $(FLOWLIB)
	rebuild -no-links $(INCLUDE_OPTS) $(LIB_OPTS) -lflags "$(LINKER_FLAGS)" -package compiler-libs.common src/main.native

%.h: $(subst _build/,,$@)
	mkdir -p $(dir $@)
	cp $(subst _build/,,$@) $@

# Compile each object file. Equivalent to this ocamlbuild rule, but faster:
# ocamlbuild -ocamlc "ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS)" $(subst _build/,,$@)
$(BUILT_C_FILES): _build/%.c: %.c
	mkdir -p $(dir $@)
	cp $< $@
$(BUILT_OBJECT_FILES): %.o: %.c $(ALL_HEADER_FILES)
	cd $(dir $@) && ocamlopt $(EXTRA_INCLUDE_OPTS) $(CC_OPTS) -c $(notdir $<)

hack/utils/get_build_id.gen.c: FORCE scripts/utils.ml scripts/gen_build_id.ml
	ocaml -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

_build/hack/utils/get_build_id.gen.c: FORCE scripts/utils.ml scripts/gen_build_id.ml
	ocaml -I scripts -w -3 unix.cma scripts/gen_build_id.ml $@

# We only rebuild the flowlib archive if any of the libs have changed. If the
# archive has changed, then the incremental build needs to re-embed it into the
# binary. Unfortunately we rely on ocamlbuild to embed the archive on OSX and
# ocamlbuild isn't smart enough to understand dependencies outside of its
# automatic-dependency stuff.
$(FLOWLIB): $(wildcard lib/*)
	mkdir -p bin
	tar czf $@ -C lib .
ifeq ($(OS), Darwin)
	rm -f _build/src/flow.d.byte _build/src/flow.native
	rm -f _obuild/flow/flow.byte _obuild/flow/flow.asm
endif

FORCE:

.PHONY: all build-flow
