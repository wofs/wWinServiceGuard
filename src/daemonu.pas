unit DaemonU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, mServiceGuard, simpletimer,
  DaemonApp, dateutils, CTimer;

type

  { TDaemon1 }

  TDaemon1 = class(TDaemon)
    TimerPool: TSimpleTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
    procedure TimerPoolTimer(const Sender: TObject);
  private
    procedure onEndThread(Sender: TObject);

  protected
    fServiceGuard: TServiceGuard;
  public

  end;

  { TDaemonThread }

  TDaemonThread = Class(TThread)
    private
      fOnEnd: TNotifyEvent;
      fServiceGuard: TServiceGuard;

    public
      procedure Execute; override;

      property onEnd: TNotifyEvent read fOnEnd write fOnEnd;
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
begin
  repeat
    if fServiceGuard.Status = gsReady then
    begin
      try
        fServiceGuard.Run;
      finally
        if Assigned(fOnEnd) then fOnEnd(self);
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

  fServiceGuard.Init;

  fDaemonThread.Start;
end;

procedure TDaemon1.DataModuleCreate(Sender: TObject);
begin
  TimerPool.OnTimer:=@TimerPoolTimer;
end;

procedure TDaemon1.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  TimerPool.Enabled:= false;

  fServiceGuard.Finish;

  fDaemonThread.Terminate;
  fServiceGuard.WriteLog('['+ApplicationName+'] was stopped.', ltSystem);
  FreeAndNil(fServiceGuard);
end;

procedure TDaemon1.TimerPoolTimer(const Sender: TObject);
begin
  fServiceGuard.WriteLog('TimerPoolTimer =>', ltDevelop, true);

  TimerPool.Enabled:= false;

  if fDaemonThread.Suspended then
    fDaemonThread.Start;

  fServiceGuard.WriteLog('TimerPoolTimer |', ltDevelop, true);
end;

procedure TDaemon1.onEndThread(Sender: TObject);
begin
  fServiceGuard.WriteLog('onEndThread =>', ltDevelop, true);

  TimerPool.Enabled:= true;

  TDaemonThread(Sender).Suspended:= true;

  fServiceGuard.WriteLog('onEndThread |', ltDevelop, true);
end;

initialization
  RegisterDaemon;
end.

