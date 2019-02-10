# wWinServiceGuard
Allows you to control and start selected services, as well as restart them in case of a fall.

### The project uses:
* http://www.lazarus-ide.org/  
* https://github.com/wofs/wWinServices.git  

### Ini file has the following format:  
[Settings]				; Program setting  
PollingPeriod=1			; Service status polling period  
StartServices=1			; Allow service management (start / stop)  
LogMode=3				; Event logging level. 0-off, 1-Errors, 2-System, 3 - Debugging  
  
[ControlledServices]	; Controlled services  
s0=W32Time  				
s1=Mcx2Svc  
  
[NotStopServices]		; Services that are forbidden to stop.  	
s0=W32Time  
