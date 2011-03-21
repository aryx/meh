
VERSION = 1.2.0

OCAMLC     = ocamlc.opt
OCAMLOPT   = ocamlopt.opt
OCAMLMKLIB = ocamlmklib
OCAMLLIB   = /home/pad/packages/stow/godi-3.11/lib/ocaml/std-lib
OCAMLDOC   = ocamldoc
OCAMLDEP   = ocamldep

INSTALLDIR = $(OCAMLLIB)/cairo

LABLGTKDIR = ../../ocamlgtk/src
C_LABLGTKDIR = $(subst +,$(OCAMLLIB)/,$(LABLGTKDIR))

# stop ocamlmklib moaning
FILT = -Wl,--export-dynamic

CAIRO_CFLAGS = -I/usr/include/cairo -I/usr/include/freetype2 -I/usr/include/libpng12  
CAIRO_LIBS   = $(filter-out $(FILT),-L/usr/lib -lcairo -lfreetype  )

GDK_CFLAGS = -I/usr/include/cairo -I/usr/include/freetype2 -I/usr/include/libpng12 -I/usr/include/gtk-2.0 -I/usr/lib/gtk-2.0/include -I/usr/include/pango-1.0 -I/usr/include/glib-2.0 -I/usr/lib64/glib-2.0/include  
GDK_LIBS   = $(filter-out $(FILT),-L/usr/lib -L/lib64 -lgdk-x11-2.0 -lpangocairo-1.0 -lpango-1.0 -lcairo -lgdk_pixbuf-2.0 -lm -lgobject-2.0 -lgmodule-2.0 -ldl -lglib-2.0  )

LIBSVG_CAIRO_CFLAGS = 
LIBSVG_CAIRO_LIBS   = 

cobjs     = $(patsubst %.c, %.o, $(filter %.c,$(1)))
mlintfs   = $(patsubst %.mli, %.cmi, $(filter %.mli,$(1)))
mlobjs    = $(patsubst %.ml, %.cmo, $(filter %.ml,$(1)))
mloptobjs = $(patsubst %.ml, %.cmx, $(filter %.ml,$(1)))
