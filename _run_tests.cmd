@echo off
echo Running tests...

echo ---------- Errors ----------
echo Test for checking sub main:
guu_dbg tests\test1.guu /run
echo Test for outsides with spaces:
guu_dbg tests\test2.guu /run
echo Test for undefined tokens:
guu_dbg tests\test3.guu /run
echo Test for invalid calls:
guu_dbg tests\test4.guu /run

echo ---------- Passes ----------
echo Run sample1.guu:
guu_dbg samples\sample1.guu /run

pause