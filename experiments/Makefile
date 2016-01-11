#MWH: these were in the submission but I don't think they are good
#choices because they contain no synchronization operations
#  consolemap_comb.c \
#  ide-disk_comb.c \

#MWH: this one I haven't tried
#  serial_core_comb.c \

#MWH: I'm quite confident in the main() for the first two of these
PAPERDRIVERS := \
  plip.ko_merged.c \
  wavelan.ko_merged.c \
  esp-2.5.62.c \
  synclink.ko_merged.c \

DRIVERS := \
  $(PAPERDRIVERS) \
  forcedeth_comb.c \
  nsd_comb.c \
  tty_io_comb.c \
  3c523_comb.c \
  yellowfin_comb.c

# TODO: split these into the ones in the paper and otherwise

SMALLAPPS := \
  aget_comb.c \
  knot_comb.c \
  ctrace_comb.c \
  pfscan_comb.c \
  engine.c \
  smtprc_comb.c

BIGAPPS := \
  freshclam_comb.c \
  sigtool_comb.c \
  clamd_comb.c 

# NEW APPS
#  mt-daapd_nocodec.c \
#  icecast-2.3.1.c \
#  retawq_comb.c \
#  mt-daapd_comb.c \
#  httpd_comb.c 

# OLD APPS; probably won't use
#  clamdscan_comb.c \
#  libdb-example2_comb.c \
#  libdb-example_comb.c \
#  clamscan_comb.c \
#  knot_comb_plhash.c   VERSION THAT CONTAINS HASH TABLE

QUICK := $(addsuffix _lib.c,$(basename $(SMALLAPPS)))
ALLAPPS := $(addsuffix _lib.c,$(basename $(SMALLAPPS) $(BIGAPPS)))
TESTS := $(ALLAPPS) $(DRIVERS)

######################################################################

CILLY = ../cil/bin/cilly 
CILARGS = -c --list-guardedby --list-shared --locksmith-version $(OPTS)

ECHO = @

all: init timestamp $(TESTS:%.c=out/%.out)

quick: init timestamp $(QUICK:%.c=out/%.out)

timestamp:
	$(ECHO) date >> times.txt
	$(ECHO) echo ============================ >> times.txt

init:
#	$(ECHO) $(RM) -r out
	$(ECHO) test -d out || mkdir out
	$(ECHO) test -f core && $(RM) -f core || true

loc:
	for file in $(SMALLAPPS) $(DRIVERS); do \
	echo ===== $$file =====; \
	wc -l $$file lib.c; echo; done

clean:
	$(RM) core a.out graph*.dot *graph.ps a.out *~
	$(RM) out/* diffs/*

%_lib.c: %.c
	$(ECHO) CILLY_DONT_COMPILE_AFTER_MERGE=1 \
	CILLY_DONT_LINK_AFTER_MERGE=1 \
	$(CILLY) --merge --mergedout=$@ --dontlocksmith $< lib.c \
	> /dev/null 2>&1

#$(ECHO) cat $^ lib.c > $@

out/%.out: %.c
	$(ECHO) 
	$(ECHO) echo -n $<
	$(ECHO) echo '$(CILLY) $< > $@ 2>&1' > $@
	$(ECHO) TIME=`./test.sh /dev/null $@ $@ $(CILLY) $(CILARGS) $<`; \
	  echo -e "\\033[30G \\033[1;32m done("`grep "Possible data race" $@ | wc -l`") \\033[0;39m"; \
	  echo $(basename $<) $$TIME `grep "Possible data race" $@ | wc -l` >> times.txt
	$(ECHO) cat $@ | grep -v "/tmp/" | \
		grep -v "LockSmith run for" | \
		grep -v "\`__unique__' attribute directive ignored" | \
		sed -e 's/#[0-9]*\\n//g' > /tmp/outX$$;
	$(ECHO) cat ok/$(basename $<).out | grep -v "/tmp/" | \
		grep -v "LockSmith run for" | \
		grep -v "\`__unique__' attribute directive ignored" | \
		sed -e 's/#[0-9]*\\n//g' > /tmp/outY$$;
	$(ECHO) if diff -Bb /tmp/outY$$ /tmp/outX$$ > /tmp/out$$ 2>&1; \
		then $(RM) /tmp/out$$ /tmp/outX$$ /tmp/outY$$; true; \
		else \
			$(RM) /tmp/outX$$ /tmp/outY$$; \
			echo -e "\\033[30G \\033[1;31m output differs \\033[0;39m";  \
			echo -e "\\033[30G \\033[1;31m old: " `grep "Possible data race" ok/$(basename $<).out | wc -l`"  new: " `grep "Possible data race" out/$(basename $<).out | wc -l`" \\033[0;39m"; \
			mkdir -p diffs; mv /tmp/out$$ diffs/$(basename $<).diff; true; \
	fi && \
	echo -e "\\033[30G \\033[1;32m ok \\033[0;39m"

# 	$(ECHO) 
# 	$(ECHO) echo -n $<
# 	$(ECHO) echo '$(CILLY) $< > $@ 2>&1' >> times.txt
# 	$(ECHO) /usr/bin/time -a -otimes.txt $(CILLY) $< > $@ 2>&1 || true
# 	$(ECHO) echo -e "\\033[30G \\033[1;32m done("`grep "Possible data race" $@ | wc -l`") \\033[0;39m"
# 	$(ECHO) echo `grep "Possible data race" $@ | wc -l` races reported >> times.txt
# 	$(ECHO) echo >> times.tex

