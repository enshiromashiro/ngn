## c/c++ app makefile

include make.conf


#executable
EXE = ngn

#object files
SRC = ngn.cpp
OBJS = $(patsubst %.c, %.o, $(SRC))


#vpath
#GPATH = $(SRCDIR):$(TMPDIR)
#VPATH = $(SRCDIR):$(TMPDIR)

## miscellous
all: $(EXE)

clean:
	RM -rf $(BINDIR)/*
#	RM -rf $(TMPDIR)/*
	RM -rf *.o


## surfix rules
.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@


## dependence
$(EXE): $(OBJS) $(BINDIR)
	$(CXX) $(LDFLAGS) -o $(BINDIR)/$@ $(filter-out bin, $<)


ngn.o: ngn.cpp
ngn.cpp: ngn.h Options.h
ngn.h: debug.h


#directories
$(BINDIR):
	mkdir $(BINDIR)

#$(TMPDIR):
#	mkdir $(TMPDIR)
