#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=test.ml main.ml 

TARGET=lib

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=vvv_console
PROGS+=vvv_test

ifeq ($(FEATURE_GUI),1)
PROGS+=meh
GUIEXECDIR=gui
endif

OPTPROGS= $(PROGS:=.opt)

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

#format: XXXDIR, XXXCMD, XXXCMDOPT, XXXINCLUDE (if different XXXDIR), XXXCMA
#template: 
#  ifeq ($(FEATURE_XXX), 1)
#  XXXDIR=xxx
#  XXXCMD= $(MAKE) -C xxx &&  $(MAKE) xxx -C commons
#  XXXCMDOPT= $(MAKE) -C xxx &&  $(MAKE) xxx.opt -C commons
#  XXXCMA=xxx/xxx.cma  commons/commons_xxx.cma
#  XXXSYSCMA=xxx.cma
#  XXXINCLUDE=xxx
#  else
#  XXXCMD=
#  XXXCMDOPT=
#  endif

ifeq ($(FEATURE_BACKTRACE), 1)
BTCMD= $(MAKE) backtrace -C commons
BTCMDOPT= $(MAKE) backtrace.opt -C commons
BTCMA=commons/commons_backtrace.cma
else
endif

ifeq ($(FEATURE_GUI),1)
GUIDIR=external/ocamlgtk
GUICMD= $(MAKE) all -C $(GUIDIR) && $(MAKE) gui       -C commons 
GUICMDOPT= $(MAKE) opt -C $(GUIDIR) && $(MAKE) gui.opt       -C commons;
GTKINCLUDE=external/ocamlgtk/src
CAIRODIR=external/ocamlcairo
CAIROINCLUDE=external/ocamlcairo/src
endif

ifeq ($(FEATURE_PCRE), 1)
REGEXPDIR=external/ocamlpcre
REGEXPCMD= $(MAKE) -C $(REGEXPDIR) &&  $(MAKE) regexp -C commons
REGEXPCMDOPT= $(MAKE) -C $(REGEXPDIR) &&  $(MAKE) regexp.opt -C commons
REGEXPCMA=external/ocamlpcre/lib/pcre.cma  commons/commons_regexp.cma
PCREINCLUDE=external/ocamlpcre/lib
endif

# could be under some ifeq($(FEATURE_WEB))
OCAMLNETDIR=external/ocamlnet
OCAMLNETINCLUDE=external/ocamlnet/netsys external/ocamlnet/netstring
OCAMLNETCMA= \
  external/ocamlnet/netsys/netsys_oothr.cma \
  external/ocamlnet/netsys/netsys.cma \
  external/ocamlnet/netstring/netstring.cma \

#  external/ocamlnet/netstring/netaccel.cma \

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=bigarray.cma nums.cma str.cma unix.cma $(XXXSYSCMA)

LIBS= commons/commons.cma \
      $(BTCMA) \
      $(REGEXPCMA) \
      $(OCAMLNETCMA) \
      commons/commons_features.cma \
      globals/globals.cma \
     h_program-lang/lib.cma \
    lang_html/parsing/lib.cma \
     lang_html/analyze/lib.cma \
    lang_js/parsing/lib.cma \
     lang_js/analyze/lib.cma \
    lang_css/parsing/lib.cma \
    lang_web/parsing/lib.cma \

MAKESUBDIRS=commons \
  $(XXXDIR) $(REGEXPDIR) \
  $(GUIDIR) $(CAIRODIR) \
  $(OCAMLNETDIR) \
  h_visualization \
  h_program-lang \
  globals \
  lang_js/parsing \
   lang_js/analyze \
  lang_html/parsing \
   lang_html/analyze \
  lang_css/parsing \
  lang_web/parsing \

INCLUDEDIRS=$(MAKESUBDIRS) \
 commons/ocamlextra commons/ocollection \
 commons/lib-json commons/lib-xml commons/lib-sexp \
 $(GTKINCLUDE) $(CAIROINCLUDE) $(PCREINCLUDE) $(OCAMLNETINCLUDE)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all all.opt opt top clean distclean

#note: old: was before all: rec $(EXEC) ... but can not do that cos make -j20
#could try to compile $(EXEC) before rec. So here force sequentiality.

all:: Makefile.config
	$(MAKE) rec 
	$(MAKE) $(PROGS) 
opt:
	$(MAKE) rec.opt 
	$(MAKE) $(OPTPROGS) 
all.opt: opt
top: $(TARGET).top

rec:
	$(MAKE) -C commons 
	$(XXXCMD)
	$(REGEXPCMD)
	$(BTCMD)
	$(MAKE) features -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	$(MAKE) all.opt -C commons 
	$(REGEXPCMDOPT)
	$(BTCMDOPT)
	$(MAKE) features.opt -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 

$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^

$(TARGET).top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^



clean::
	rm -f $(TARGET)
clean:: 
	rm -f $(TARGET).top
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 
clean::
	rm -f *.opt


depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config.ml
	rm -f TAGS
#	find -name ".#*1.*" | xargs rm -f

# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)


#------------------------------------------------------------------------------
# xxx target
#------------------------------------------------------------------------------

#template:
#xxx: $(LIBS) main_xxx.cmo 
#	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
#
#xxx.opt: $(LIBS:.cma=.cmxa) main_xxx.cmx
#	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^
#
#clean::
#	rm -f xxx

#------------------------------------------------------------------------------
# vvv_console target
#------------------------------------------------------------------------------

vvv_console: $(LIBS) main.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

# could be BASICSYSIBS at some point
vvv_console.opt: $(LIBS:.cma=.cmxa) main.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

clean::
	rm -f vvv_console

#------------------------------------------------------------------------------
# meh target
#------------------------------------------------------------------------------

meh: $(LIBS) main_meh.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

meh.opt: $(LIBS:.cma=.cmxa) main_meh.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

clean::
	rm -f meh

#------------------------------------------------------------------------------
# vvv_test targets
#------------------------------------------------------------------------------

vvv_test: $(LIBS) main_test.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

vvv_test.opt: $(LIBS:.cma=.cmxa) main_test.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

clean::
	rm -f vvv_test

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

# note: don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp -a config/ $(DESTDIR)$(SHAREDIR)
	@echo ""
	@echo "You can also install XXX by copying the program XXX"
	@echo "(available in this directory) anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -rf $(DESTDIR)$(SHAREDIR)/config/

version:
	@echo $(VERSION)



##############################################################################
# Package rules
##############################################################################

PACKAGE=$(TARGET)-$(VERSION)
TMP=/tmp


package: 
	make srctar 

srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz  --exclude=CVS --exclude=_darcs  $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)



##############################################################################
# Website rules
##############################################################################

WEBSITE=/home/pad/mobile/homepage/software/project-xxx

gen-html:
	emacs -l ~/.emacs --eval "(progn (htmlize-many-files '(\"changes.txt\")) (kill-emacs))"

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)

#	make gen-html
#	cp changes.txt.html $(WEBSITE)/changes-$(VERSION).html

##############################################################################
# Developer rules
##############################################################################

PFFFBIN=/home/pad/pfff

.PHONY:: tags db layers   visual

tags:
	$(PFFFBIN)/stags -verbose -lang ml .
db:
	$(PFFFBIN)/pfff_db -verbose  -lang ml -o DB_LIGHT.marshall .
visual:
	$(PFFFBIN)/codemap -profile -ss 2 \
	   -with_info DB_LIGHT.marshall -with_layers . -ocaml_filter .

test:
	./$(TARGET)_test all
push:
	git push origin master
pull:
	git pull

archi:
	echo "TODO"

##############################################################################
# Pad specific rules
##############################################################################

# darcs forest
