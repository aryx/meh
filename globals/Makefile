TOP=..
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=globals

SRC= flag.ml config.ml

LIBS=
INCLUDEDIRS=../commons
#INCLUDES=-I ../commons

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common


##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma

all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


config.ml:
	@echo "config.ml is missing. Have you run ./configure?"
	@exit 1

distclean::
	rm -f config.ml
