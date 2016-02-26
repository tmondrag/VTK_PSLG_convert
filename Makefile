# compiler
FC = gfortran

# Compiler flags
## Linking step
### release version
#FCLFLAGS = -O3
#### debug version
FCLFLAGS = -g -Wall -fbounds-check -fbacktrace
## Compile step
### release version
#FCFLAGS = -O3
### debug version
FCFLAGS = -g -Wall -fbounds-check -fbacktrace

# Directories
BINDIR = bin
OBJDIR = obj
SRCDIR = src
MODDIR = mod

# add the module directory path to flags
## in ifort
#FCFLAGS += -module $(MODDIR)
#FCLFLAGS += -module $(MODDIR)
## in gfortran
FCFLAGS += -J$(MODDIR)

# Programs - executables which will be built
PROGRAMS = $(addprefix $(BINDIR)/,kindtest convert_PSLG_to_VTK)

# default Make target
all : $(PROGRAMS) 

# Executable dependencies - list object files which each executable will need for linker step
$(BINDIR)/kindtest : $(addprefix $(OBJDIR)/,kindprecision.o kindtest.o)
$(BINDIR)/convert_PSLG_to_VTK : $(addprefix $(OBJDIR)/,kindprecision.o filehandling.o types.o input.o output.o convert_PSLG_to_VTK.o)

# Object dependencies - some objects depend on modules from other objects to be compiled first
$(OBJDIR)/kindtest.o : $(OBJDIR)/kindprecision.o
$(OBJDIR)/convert_PSLG_to_VTK.o : $(addprefix $(OBJDIR)/,filehandling.o types.o kindprecision.o input.o output.o)
$(OBJDIR)/types.o : $(OBJDIR)/kindprecision.o
$(OBJDIR)/input.o :  $(addprefix $(OBJDIR)/,filehandling.o types.o kindprecision.o)
$(OBJDIR)/output.o :  $(addprefix $(OBJDIR)/,filehandling.o types.o kindprecision.o)

#Default executable recipes
$(BINDIR)/%: $(OBJDIR)/%.o | $(BINDIR)
	$(FC) $(FCLFLAGS) -o $@ $^

$(OBJDIR)/%.o: $(SRCDIR)/%.f90 | $(OBJDIR) $(MODDIR)
	$(FC) $(FCFLAGS) -o $@ -c $<

# File structure generation
$(BINDIR):
	mkdir $(BINDIR)

$(OBJDIR):
	mkdir $(OBJDIR)

$(MODDIR):
	mkdir $(MODDIR)

# use this rule to move your sourcefiles into a new sourcefile directory. I haven't figured out how to make this automatic.
# example: make src
$(SRCDIR):
	mkdir $(SRCDIR)
	mv -u *.f90 $(SRCDIR)/
	
# Utility targets
.PHONY: all cleanobj cleanbin clean distclean

cleanobj:
	rm -f $(MODDIR)/*.mod $(OBJDIR)/*.o $(OBJDIR)/*~

cleanbin:
	rm -f $(BINDIR)/*

clean: cleanbin cleanobj
	rm -f *~ $(SRCDIR)/*~
	
distclean: clean
	rm -rf $(MODDIR) $(OBJDIR) $(BINDIR)