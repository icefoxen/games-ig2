HC=ghc
FLAGS=

THRIFT=thrift
THRIFTFLAGS=--gen hs -o src/
THRIFTRESULTS=src/gen-hs/TurnProtocol_Consts src/gen-hs/TurnProtocol_Types
THRIFTSRC=$(addsuffix .hs, $(THRIFTRESULTS))


BASES=src/UniverseGen src/Main src/Colony src/Empire src/Fleet src/Loader src/Universe src/Util $(THRIFTRESULTS)
THRIFTFILE=src/TurnProtocol.thrift

SRC=$(addsuffix .hs, $(BASES))
OBJ=$(addsuffix .o, $(BASES))
INTERFACES=$(addsuffix .hi, $(BASES))
OUTPUT=ig-serve
BUILD=bin

all: $(OUTPUT)

go: $(OUTPUT)
	./ig-serve

distclean:
	rm -rf *~ data/*~ data/*/*~ src/*~ $(OBJ) $(INTERFACES) src/gen-hs

clean: distclean
	rm -f $(OUTPUT) bin/*

# Thrift things...
# There might be a more elegant way of doing this?
# This is decent.
$(THRIFTSRC): $(THRIFTFILE)
	$(THRIFT) $(THRIFTFLAGS) $(THRIFTFILE)

# Directory bin/ needs to exist...
$(OUTPUT): $(SRC)
	rm -rf $(OUTPUT)
	$(HC) --make -o $(OUTPUT) $(SRC)
	mv $(OUTPUT) $(BUILD)
	ln -s $(BUILD)/$(OUTPUT) $(OUTPUT)
