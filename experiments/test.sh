#!/bin/sh
# test.sh: test.sh [-k] PROG_INFILE PROG_OUTFILE PROG_ERRFILE PROG [ARG]*
#   the elapsed time that results from running the program is output on stdout
#   run with -k if you expect to have to kill the program rather than letting
#     it run to completion (e.g., for a server throughput test)
#   returns 0 on success, else a nonzero return code

if [ "$1" = "-k" ]; then
KILL=y
shift
else
KILL=
fi

if [ $# -lt 4 ]; then
  echo insufficient arguments
  exit 1;
fi

INFILE=$1
shift
OUTFILE=$1
shift
ERRFILE=$1
shift
TMP=/tmp/test_sh$$
if [ -z "$KILL" ]; then
#  echo "/usr/bin/time -f "%e" -o $TMP -a $* <$INFILE >> $OUTFILE 2>> $ERRFILE"
  /usr/bin/time -f "%e" -o $TMP -a $* <$INFILE >> $OUTFILE 2>> $ERRFILE
else
#  echo "/usr/bin/time -f "%e" -o $TMP -a $* <$INFILE > $OUTFILE 2>> $ERRFILE"
  /usr/bin/time -f "%e" -o $TMP -a $* <$INFILE > $OUTFILE 2>> $ERRFILE
fi
if [ $? != 0 ]; then
  rm -f $TMP
  echo "FAILED-$2"
  exit $?
else
  cat $TMP
  rm -f $TMP
fi
