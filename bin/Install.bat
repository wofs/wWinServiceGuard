@echo Installing...
start %~dp0ServiceGuard.exe -i
@echo off
TIMEOUT /T 2 /NOBREAK
@echo on
@echo Starting...
net start Service.Guard
@pause