unit DaemonU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, mServiceGuard, simpletimer,
  DaemonApp, dateutils, CTimer;

type
  TDaemonThread = class;

  TDaemonEvent = procedure(Sender: TDaemonThread; const aRunMode:TRunMode) of object;

  { TDaemon1 }

  TDaemon1 = class(TDaemon)
    TimerSystem: TSimpleTimer;
    TimerRestartServices: TSimpleTimer;
    TimerPool: TSimpleTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
    procedure TimerPoolTimer(const Sender: TObject);
    procedure TimerRestartServicesTimer(const Sender: TObject);
    procedure TimerSystemTimer(const Sender: TObject);
  private
    procedure onEndThread(Sender: TDaemonThread; const aRunMode:TRunMode);

  protected
    fServiceGuard: TServiceGuard;
  public

  end;

  { TDaemonThread }

  TDaemonThread = Class(TThread)
    private
      fOnEnd: TDaemonEvent;
      fServiceGuard: TServiceGuard;
    public
      procedure Execute; override;

      property onEnd: TDaemonEvent read fOnEnd write fOnEnd;
 end;

var
  fDaemon1: TDaemon1;
  fDaemonThread:TDaemonThread;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TDaemon1)
end;

{$R *.lfm}

{ TDaemonThread }

procedure TDaemonThread.Execute;
var
  aRunMode: TRunMode;
begin
  repeat
    if fServiceGuard.Status = gsReady then
    begin
      try
        aRunMode:= fServiceGuard.Run;
      finally
        if Assigned(fOnEnd) then fOnEnd(self, aRunMode);
      end;
    end;
  until Terminated;
end;

{ TDaemon1 }

procedure TDaemon1.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  fServiceGuard:= TServiceGuard.Create;

  fServiceGuard.WriteLog('', ltSystem, false);

  fServiceGuard.WriteLog('['+ApplicationName+'] was launched.', ltSystem);
  fServiceGuard.WriteSettingsToLog;

  fDaemonThread:= TDaemonThread.Create(true);
  fDaemonThread.fServiceGuard:= fServiceGuard;
  fDaemonThread.onEnd:=@onEndThread;

  TimerPool.Interval:= fServiceGuard.Settings.PollingPeriod*1000;
  TimerSystem.Interval:= 100;

  fServiceGuard.SetQueue(rmInit);

  TimerSystem.Enabled:= true;
end;

procedure TDaemon1.DataModuleCreate(Sender: TObject);
begin
  TimerSystem.OnTimer:=@TimerSystemTimer;
  TimerPool.OnTimer:= @TimerPoolTimer;
  TimerRestartServices.OnTimer:= @TimerRestartServicesTimer;
end;

procedure TDaemon1.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  TimerPool.Enabled:= false;
  TimerRestartServices.Enabled:= false;
  fServiceGuard.SetQueue(rmFinish);

  while Assigned(fServiceGuard) do
     sleep(500);
end;

procedure TDaemon1.TimerPoolTimer(const Sender: TObject);
begin
  fServiceGuard.WriteLog('TimerPool =>', ltDevelop, true);
  TimerPool.Enabled:= false;

  fServiceGuard.SetQueue(rmMonitoring);

  fServiceGuard.WriteLog('TimerPool |', ltDevelop, true);
end;

procedure TDaemon1.TimerRestartServicesTimer(const Sender: TObject);
begin
  fServiceGuard.WriteLog('TimerRestartServices =>', ltDevelop, true);
  TimerRestartServices.Enabled:= false;

  fServiceGuard.SetQueue(rmRestart);

  fServiceGuard.WriteLog('TimerRestartServices |', ltDevelop, true);
end;

procedure TDaemon1.TimerSystemTimer(const Sender: TObject);
begin
if not fServiceGuard.QueueIsEmpty and fDaemonThread.Suspended then
  fDaemonThread.Start;
end;

procedure TDaemon1.onEndThread(Sender: TDaemonThread; const aRunMode: TRunMode);
begin
  fServiceGuard.WriteLog('onEndThread =>', ltDevelop, true);

  case aRunMode of
    rmMonitoring: TimerPool.Enabled:= true;
    rmRestart: TimerRestartServices.Enabled:= true;
    rmInit:
      begin
        if fServiceGuard.Settings.RestartPeriod>0 then
        begin
          TimerRestartServices.Interval:= fServiceGuard.Settings.RestartPeriod*1000;
          TimerRestartServices.Enabled:= true;
        end;

        TimerPool.Enabled:= true;
      end;
    rmFinish:
      begin
        TimerSystem.Enabled:= false;
        fDaemonThread.Terminate;
        fServiceGuard.WriteLog('['+ApplicationName+'] was stopped.', ltSystem);
        FreeAndNil(fServiceGuard);
      end;
  end;

  if not (aRunMode = rmFinish) then
    TDaemonThread(Sender).Suspended:= true;

  fServiceGuard.WriteLog('onEndThread |', ltDevelop, true);
end;

initialization
  RegisterDaemon;
end.

