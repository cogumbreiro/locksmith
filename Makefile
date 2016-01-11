LOCKSMITH_MODULES = \
	lockutil \
	worklist \
	dotpretty \
	bansheemlifc \
	falsecfl \
	mycfl \
	livevars \
	locksettings \
	uniqueness \
	labelname \
	labelflow \
	controlflow \
	lockprofile \
	lockstate \
	shared \
	correlation \
	semiunification \
	locktype \
	lockpick \
#	locksmith

LOCKSMITH_CMODULES = bansheeifc
CP4S        += locksettings.p4

BANSHEE = $(PWD)/banshee
DYCKCFL_DIR = $(BANSHEE)/dyckcfl
ENGINE_DIR = $(BANSHEE)/engine
REGION_DIR = $(BANSHEE)/libcompat
LINKFLAGS = \
	     $(DYCKCFL_DIR)/dyckcfl.o $(DYCKCFL_DIR)/mr_dyckcfl.o \
	     $(REGION_DIR)/libregions.a \
	     $(ENGINE_DIR)/libnsengine.a
CAML_CFLAGS := -ccopt -I$(DYCKCFL_DIR) -ccopt -I$(REGION_DIR) -ccopt -I$(ENGINE_DIR)

export LOCKSMITH_MODULES
export LOCKSMITH_CMODULES
#export LINKFLAGS
export CAML_CFLAGS
export CP4S

all:
	$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 all
	LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil

profile:
	$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 DEBUG=1 DEBUG_RALLOC=1 all
	LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil PROFILE=1

clean:
	$(MAKE) -C banshee clean
	$(MAKE) -C cil clean
