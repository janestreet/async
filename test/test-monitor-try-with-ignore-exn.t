  $ $TESTDIR/monitor_try_with_ignore_exn.exe 2>&1 \
  >    | grep -qF 'Exception raised to Monitor.try_with that already returned'
