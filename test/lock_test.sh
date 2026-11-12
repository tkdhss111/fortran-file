#!/usr/bin/env bash
# lock_test.sh — cross-process checks for lock_path / unlock_path (Linux).
#   1. a held lock makes a bounded wait time out
#   2. a blocking wait gets the lock when the holder releases it
#   3. SIGKILL of the holder releases the lock even while its child process runs (O_CLOEXEC)
#   4. a second lock_path in the same process on the same file does not get the lock
set -u
cd "$(dirname "$0")"; ifx -o lock_test ../src/file_mo.f90 lock_test.f90 || exit 1
fail=0; chk () { [ "$2" = "$3" ] && echo "ok   $1" || { echo "FAIL $1: got '$2'"; fail=1; }; }
./lock_test hold 2 >/dev/null & sleep 0.3
chk "bounded wait times out"   "$(./lock_test try 0.5)" "try: stat=1"
chk "blocking wait acquires"   "$(./lock_test block)"   "block: acquired"; wait
./lock_test hold 30 >/dev/null & P=$!; sleep 0.3; kill -9 $P; sleep 0.1
chk "released on SIGKILL"      "$(./lock_test try 1)"   "try: stat=0"
chk "nested lock not granted"  "$(./lock_test nested)"  "nested second: stat=1"
rm -f lock_test t.lock *.mod; exit $fail
