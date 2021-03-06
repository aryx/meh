TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_html.ml \
     ast_html.ml \
     meta_ast_html.ml \
     visitor_html.ml \
     lib_parsing_html.ml \
     export_html.ml \
     parser_html.ml lexer_html.ml \
     token_helpers_html.ml \
     dtd_simple.ml \
     parse_html.ml \
     unparse_html.ml \
     test_parsing_html.ml



SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/commons.cma

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(TOP)/commons/lib-sexp $(TOP)/commons/lib-json \
 $(TOP)/globals \
 $(TOP)/h_program-lang \
 $(TOP)/external/ocamlnet/netsys \
 $(TOP)/external/ocamlnet/netstring \


##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


lexer_html.ml: lexer_html.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_html.ml
beforedepend:: lexer_html.ml


parser_html.ml parser_html.mli: parser_html.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_html.ml parser_html.mli parser_html.output
beforedepend:: parser_html.ml parser_html.mli


visitor_html.cmo: visitor_html.ml
	$(OCAMLC) -w y -c $<


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
