@echo off
call build.cmd generatecoveragereport
if "%ERRORLEVEL%" NEQ "0" (
    echo"ErrorLevel:%ERRORLEVEL%"
    echo "attempting to force generate coverage report"
    call build.cmd generatecoveragereport -s
) else (
    echo"ErrorLevel:%ERRORLEVEL%"
)