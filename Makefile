all:
#	$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 DEBUG=1 DEBUG_RALLOC=1 all
	$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 all
	$(MAKE) -C cil

clean:
	$(MAKE) -C banshee clean
	$(MAKE) -C cil clean
