unit mServiceGuard;

{$mode objfpc}{$H+}

{
  wofs(c)2019 [wofssirius@yandex.ru]
  GNU LESSER GENERAL PUBLIC LICENSE v.2.1
}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils, vFunc, wWinServices,
  gvector, IniFiles, daemonapp;

type
  TMsgOutputChannel = (mocFile);
  TGuardStatus = (gsInit, gsReady, gsWite, gsSleep, gsError);
  TControlMode = (cmRunAll, cmRestartSome, cmStart, cmStop, cmGetStatuses);

  TLogMode = (lmOff, lmError, lmSystem, lmDebug, lmDevelop);
  TLogType = (ltError, ltSystem, ltDebug, ltDevelop);

  TRunMode = (rmMonitoring, rmRestart);

  TService = record
    Index: integer;
    Name: string;
    Status: word;
    StatusText: string;
    NotStop: boolean;
    RestartPeriod: integer;
  end;

  TServices = specialize TVector<TService>;

  TSettings = record
    PollingPeriod: integer;
    RestartPeriod: integer;
    StartServices: boolean;
    MsgOutputChannel: TMsgOutputChannel;
  end;

  { TServiceGuard }
  TQueueSystem = record
    RunMode: TRunMode;
  end;

  TQueue = specialize TVector<TQueueSystem>;

  TServiceGuard = class
    private
      fQueue: TQueue;

      fSettings: TSettings;
      fLogMode: array of TLogType;

      fServices: TServices;

      fLogFile: string;
      fCurrentPath: string;
      fStatus: TGuardStatus;

      fWinServices: TWinServices;

      const
        uIniFile = 'settings.ini';
        uLogDir = 'log';

      function CheckLogType(aLogType: TLogType): boolean;
      function GetQueue: TRunMode;
      function GetQueueIsEmpty: boolean;
      function GetRestartableServices: TServices;
      function GetRunMode: TRunMode;
      function HaveServicesStopped: boolean;
      function LogTypeAsString(aLogType: TLogType): string;
      function QueueCreateItem(aRunMode: TRunMode): TQueueSystem;
      function QueuePop: TRunMode;
      procedure QueuePush(aRunMode: TRunMode);
      procedure ServiceControlRestartSome;
      procedure ServiceControlRunAll;
      procedure ServiceControl(aMode: TControlMode);
      procedure SetLogMode(aLogMode: TLogMode);

      function GetService(aName: string): TService;

      procedure LoadDefaultSettings;
      procedure ReadIniFile;
      procedure ReadLogFileName;
      function CreateServiceItem(aName: string; aIndex: integer): TService;

      procedure SetService(aName: string; aValue: TService);
      procedure SetStatus(aValue: TGuardStatus);
      procedure ServiceControlStartServices;
      function StatusAsText(aStatus: TGuardStatus): string;
      procedure ServiceControlStopServices;

      function UpdateServiceStatus(aIndex: integer): TService;

      procedure WriteLogInFile(aText: string; const WriteDate: boolean);
      procedure WriteServiceStatesToLog;

    protected
      procedure ReadSettings;
      procedure DoGettingServiceStatuses;
      procedure DoStartServices(var aServices: TServices);
      procedure DoStopServices(var aServices: TServices);

    public
      constructor Create;
      destructor Destroy; override;

      procedure WriteSettingsToLog;
      procedure WriteLog(aText: string; const aLogType: TLogType;
        const WriteTime: boolean = true);
      procedure WriteLogFmt(aText: string; const Args: array of const; const aLogType: TLogType;
        const WriteTime: boolean = true);

      function Run: TRunMode;
      procedure Init;
      procedure Finish;
      procedure WaitReady;
      procedure SetQueue(aValue: TRunMode);

      property Services: TServices read fServices write fServices;
      property Service[aName:string]: TService read GetService write SetService;
      property Settings: TSettings read fSettings;
      property Status: TGuardStatus read fStatus write SetStatus;
      //property Queue: TRunMode read GetQueue write SetQueue;
      property QueueIsEmpty: boolean read GetQueueIsEmpty;
  end;

implementation

{ TServiceGuard }

procedure TServiceGuard.ReadIniFile;
const
  _SETTINGS                = 'Settings';
  _POLLING_PERIOD          = 'PollingPeriod';
  _START_SERVICES          = 'StartServices';
  _MSG_OUTPUT_CHANNEL      = 'MsgOutputChannel';
  _CONTROLLED_SERVICES     = 'ControlledServices';
  _NOT_STOP_SERVICES       = 'NotStopServices';
  _S                       = 's%d';
  _RESTARTABLE_SERVICES    = 'RestartableServices';
  _RESTART_PERIOD          = 'RestartPeriod';
  _LOG_MODE                = 'LogMode';
var
  IniFile: TIniFile;
  i: Integer;
  aService: TService;
  aName: String;
begin
  IniFile:= TIniFile.Create(fCurrentPath+uIniFile);
  i:= 0;

  try
    with fSettings do
    begin
      PollingPeriod:= IniFile.ReadInteger(_SETTINGS,_POLLING_PERIOD,20);
      StartServices:= IniFile.ReadInteger(_SETTINGS,_START_SERVICES,0) = 1;
      MsgOutputChannel:= TMsgOutputChannel(IniFile.ReadInteger(_SETTINGS,_MSG_OUTPUT_CHANNEL,0)); // 0 - file

      SetLogMode(TLogMode(IniFile.ReadInteger(_SETTINGS,_LOG_MODE,0)));

      aName:= EmptyStr;

      while aName <> _CONTROLLED_SERVICES do
      begin
        aName:= IniFile.ReadString(_CONTROLLED_SERVICES,Format(_S,[i]),_CONTROLLED_SERVICES);
        if aName <> _CONTROLLED_SERVICES then
          Services.PushBack(CreateServiceItem(aName, i));
        inc(i);
      end;

      if Services.Size <> 0 then
        begin
          //_RESTARTABLE_SERVICES
          RestartPeriod:= IniFile.ReadInteger(_RESTARTABLE_SERVICES,_RESTART_PERIOD,0);
          i:= 0;
          aName:= EmptyStr;

          while aName <> _RESTARTABLE_SERVICES do
          begin
            aName:= IniFile.ReadString(_RESTARTABLE_SERVICES,Format(_S,[i]),_RESTARTABLE_SERVICES);

            aService:= Service[aName];
            aService.RestartPeriod:= RestartPeriod;
            Service[aName]:= aService;
            inc(i);
          end;

          //_NOT_STOP_SERVICES
          i:= 0;
          aName:= EmptyStr;

          while aName <> _NOT_STOP_SERVICES do
          begin
            aName:= IniFile.ReadString(_NOT_STOP_SERVICES,Format(_S,[i]),_NOT_STOP_SERVICES);

            aService:= Service[aName];
            aService.NotStop:= true;
            Service[aName]:= aService;
            inc(i);
          end;

        end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TServiceGuard.WriteSettingsToLog;
var
  i: Integer;
begin
 if Services.Size = 0 then
   begin
     WriteLog('Not found controlled services!',ltSystem);
     Status:= gsError;
   end
 else
   begin
     WriteLogFmt('PollingPeriod: %d s',[Settings.PollingPeriod], ltDebug, false);

     if Settings.StartServices then
       WriteLog('StartServices: True', ltDebug, false)
     else
       WriteLog('StartServices: False', ltDebug, false);

     case Settings.MsgOutputChannel of
       mocFile: WriteLog('MsgOutputChannel = File', ltDebug, false);
     end;


     WriteLog('Controlled services:', ltDebug, false);
     for i:=0 to Services.Size-1 do
       WriteLog(#9+Services[i].Name, ltDebug, false);

     WriteLog('Restartable services:', ltDebug, false);

     for i:=0 to Services.Size-1 do
       begin
         if Services[i].RestartPeriod > 0 then
           WriteLogFmt(#9+'%s [%d s]',[Services[i].Name, Services[i].RestartPeriod], ltDebug, false);
       end;

     WriteLog('Not stop services:', ltDebug, false);

     for i:=0 to Services.Size-1 do
       begin
         if Services[i].NotStop then
           WriteLogFmt(#9+'%s',[Services[i].Name], ltDebug, false);
       end;
   end;
end;

procedure TServiceGuard.LoadDefaultSettings;
begin
 with fSettings do
 begin
   PollingPeriod:= 0;
   StartServices:= false;
   MsgOutputChannel:= mocFile;
 end;
end;

function TServiceGuard.GetService(aName: string): TService;
var
  i: Integer;
begin
  Result.Name:='';

  for i:=0 to Services.Size-1 do
    if Services[i].Name = aName then
      begin
        Result:= Services[i];
        exit;
      end;
end;

procedure TServiceGuard.ReadLogFileName;
var
  aLogDir: RawByteString;
begin
 fCurrentPath := ExtractFileDir(Application.Params[0])+ DirectorySeparator;

 aLogDir:= fCurrentPath + uLogDir;
 if not DirectoryExistsUTF8(aLogDir) then ForceDirectoriesUTF8(aLogDir);

 fLogFile:= aLogDir+ DirectorySeparator +FormatDateTime('ddmmyyyy',Now)+'.log';
end;

function TServiceGuard.CreateServiceItem(aName: string; aIndex: integer): TService;
begin
  Result.Index:= aIndex;
  Result.Name:= aName;
  Result.Status:= 0;
  Result.StatusText:= '';
  Result.RestartPeriod:= 0;
  Result.NotStop:= false;
end;

procedure TServiceGuard.SetQueue(aValue: TRunMode);
begin
  QueuePush(aValue);
end;

procedure TServiceGuard.SetService(aName: string; aValue: TService);
var
  i: Integer;
begin
  for i:=0 to Services.Size-1 do
    if Services[i].Name = aName then
      begin
        Services[i]:= aValue;
        exit;
      end;
end;

function TServiceGuard.StatusAsText(aStatus: TGuardStatus):string;
begin
 case aStatus of
   gsInit    : Result:= 'gsInit';
   gsError   : Result:= 'gsError';
   gsReady   : Result:= 'gsReady';
   gsSleep   : Result:= 'gsSleep';
   gsWite    : Result:= 'gsWite';
 end;
end;

procedure TServiceGuard.SetStatus(aValue: TGuardStatus);
begin
  if fStatus=aValue then Exit;
  fStatus:=aValue;

  WriteLogFmt('Status changed: %s',[StatusAsText(aValue)], ltDevelop, true);
end;

procedure TServiceGuard.ReadSettings;
begin
 ReadLogFileName;

 if FileExistsUTF8(fCurrentPath+uIniFile) then
   ReadIniFile
 else
   LoadDefaultSettings;

end;

procedure TServiceGuard.DoGettingServiceStatuses;
var
  i: Integer;
  aService: TService;
begin
  WriteLog('GettingServiceStatuses =>', ltDevelop, true);
  WriteLog('Getting service statuses...', ltDebug);

  for i:=0 to Services.Size-1 do
    begin
      aService:= UpdateServiceStatus(i);
      WriteLogFmt(' "%s" [%s]',[aService.Name, aService.StatusText], ltDebug);
    end;

  WriteLog('Getting service statuses... [completed]', ltDebug);
  WriteLog('GettingServiceStatuses |', ltDevelop, true);
end;

function TServiceGuard.UpdateServiceStatus(aIndex: integer):TService;
begin
  Result:= Services[aIndex];
  Result.Status:= fWinServices.ServiceGetStatus('',Result.Name);
  Result.StatusText:= fWinServices.ServiceGetStatusText('',Result.Name);

  Services[aIndex]:= Result;
end;

procedure TServiceGuard.ServiceControlRunAll;
begin
  Sleep(1000);
  ServiceControlStopServices;
  Sleep(1000);
  ServiceControlStartServices;
  Sleep(1000);
end;

function TServiceGuard.GetRestartableServices:TServices;
var
  i: Integer;
begin
 Result:= TServices.Create;

 for i:=0 to Services.Size-1 do
   begin
     if Services[i].RestartPeriod>0 then
       Result.PushBack(Services[i]);
   end;
end;

function TServiceGuard.GetRunMode: TRunMode;
begin
  Result:= fQueue.Front.RunMode;
end;

procedure TServiceGuard.ServiceControlRestartSome;
var
  aServices: TServices;
begin
  WriteLog('Restarts the selected services...', ltSystem);

  aServices:= GetRestartableServices;
  try
    DoStopServices(aServices);
  finally
    aServices.Free;
  end;

  Sleep(1000);

  aServices:= GetRestartableServices;
  try
    DoStartServices(aServices);
  finally
    aServices.Free;
  end;

  WriteLog('Restarts the selected services... [completed]', ltSystem);
end;

function TServiceGuard.HaveServicesStopped:boolean;
var
  i: Integer;
begin
 Result:= false;
 for i:=0 to Services.Size-1 do
     if Services[i].Status = SC_Stopped then
     begin
       Result:= true;
       exit;
     end;
end;

procedure TServiceGuard.WriteServiceStatesToLog;
var
  i: Integer;
begin
 for i:=0 to Services.Size-1 do
   WriteLogFmt(' "%s": [%s]',[Services[i].Name, Services[i].StatusText], ltSystem);
end;

procedure TServiceGuard.WaitReady;
begin
  while not (Status = gsReady) do
    Sleep(200);
end;

procedure TServiceGuard.ServiceControl(aMode: TControlMode);
begin
  WaitReady;

  Status:= gsWite;

  try
    case aMode of
      cmRunAll:
        if HaveServicesStopped then
        begin
          WriteLog('Stopped services detected!', ltSystem);
          WriteServiceStatesToLog;
          WriteLog('All monitored services will be restarted!', ltSystem);
          ServiceControlRunAll;
        end;
      cmRestartSome     : ServiceControlRestartSome;
      cmStart           : ServiceControlStartServices;
      cmStop            : ServiceControlStopServices;
      cmGetStatuses     :
        begin
          DoGettingServiceStatuses;

          if not Settings.StartServices and HaveServicesStopped then
             begin
               WriteLog('Stopped services detected!', ltSystem);
               WriteServiceStatesToLog;
             end;
        end;
    end;

  finally
    Status:= gsReady;
  end;
end;

procedure TServiceGuard.ServiceControlStartServices;
begin
  DoStartServices(fServices);
end;

procedure TServiceGuard.DoStartServices(var aServices: TServices);
var
  i: Integer;
  aService: TService;
begin
   WriteLog('StartServices =>', ltDevelop, true);
   WriteLog('Start services...', ltSystem);

   for i:=0 to aServices.Size-1 do
     begin
       aService:= aServices[i];

       if aService.Status = SC_Stopped then
         begin
           WriteLogFmt(' "%s": [%s] %s ',[aService.Name, aService.StatusText, 'start service... '], ltSystem);

           if not fWinServices.ServiceStart('',aService.Name) then
               WriteLogFmt('"%s": %s ',[aService.Name, 'startup error'], ltSystem);

             aService:= UpdateServiceStatus(aService.Index);

             WriteLogFmt(' "%s": [%s]',[aService.Name, aService.StatusText], ltSystem);
         end;
     end;

   WriteLog('Start services... [completed]', ltSystem);
   WriteLog('StartServices |', ltDevelop, true);
end;

procedure TServiceGuard.ServiceControlStopServices;
begin
  DoStopServices(fServices);
end;

procedure TServiceGuard.DoStopServices(var aServices: TServices);
var
  i: Integer;
  aService: TService;
begin
   WriteLog('StopServices =>', ltDevelop, true);
   WriteLog('Stopping services...', ltSystem);

   for i:=aServices.Size-1 downto 0 do
     begin
       aService:= aServices[i];

       if aService.NotStop then
          WriteLogFmt(' "%s": [%s] %s ',[aService.Name, aService.StatusText, 'was skipped. This service cannot be stopped.'], ltSystem);

       if (aService.Status = SC_Running) and not aService.NotStop then
         begin
           WriteLogFmt(' "%s": [%s] %s ',[aService.Name, aService.StatusText, 'stopping service... '], ltSystem);

           if not fWinServices.ServiceStop('',aService.Name) then
               WriteLogFmt('"%s": %s ',[aService.Name,'stopping error'], ltSystem);

             aService:= UpdateServiceStatus(aService.Index);
             WriteLogFmt(' "%s": [%s]',[aService.Name, aService.StatusText], ltSystem);
         end;
     end;

   WriteLog('Stopping services... [completed]', ltSystem);
   WriteLog('StopServices |', ltDevelop, true);
end;

constructor TServiceGuard.Create;
begin
  Status:= gsInit;
  fQueue:= TQueue.Create;
  Services:= TServices.Create;
  ReadSettings;
  fWinServices:= TWinServices.Create;

  Status:= gsReady;
end;

destructor TServiceGuard.Destroy;
begin
  FreeAndNil(fQueue);
  FreeAndNil(fServices);
  FreeAndNil(fWinServices);
  inherited Destroy;
end;

function TServiceGuard.QueueCreateItem(aRunMode: TRunMode):TQueueSystem;
begin
  Result.RunMode:= aRunMode;
end;

procedure TServiceGuard.QueuePush(aRunMode: TRunMode);
begin
  fQueue.PushBack(QueueCreateItem(aRunMode));
end;

function TServiceGuard.QueuePop: TRunMode;
begin
 Result:= fQueue.Front.RunMode;
 fQueue.Erase(0);
end;

procedure TServiceGuard.WriteLogFmt(aText: string; const Args: array of const;
  const aLogType: TLogType; const WriteTime: boolean);
begin
  WriteLog(Format(aText, Args), aLogType, WriteTime);
end;


function TServiceGuard.Run: TRunMode;
begin
  WriteLog('Run =>', ltDevelop, true);

  Result:= GetRunMode;

  ServiceControl(cmGetStatuses);

  case GetQueue of
    rmMonitoring: if Settings.StartServices then ServiceControl(cmRunAll);
    rmRestart: ServiceControl(cmRestartSome);
  end;

  WriteLog('Run |', ltDevelop, true);
end;

procedure TServiceGuard.Init;
begin
 if Settings.StartServices then
   begin
     ServiceControl(cmGetStatuses);
     ServiceControl(cmStart);
   end;
end;

procedure TServiceGuard.Finish;
begin
 if Settings.StartServices then
    ServiceControl(cmStop);
end;

procedure TServiceGuard.WriteLogInFile(aText: string; const WriteDate: boolean);
var
  fsOut: TFileStream;
  aPreparedString: String;
begin
  aPreparedString:= aText+LineEnding;

  if WriteDate then
    aPreparedString:= CurrentTime +' '+ aPreparedString;

  try
    fsOut:= nil;
    if FileExistsUTF8(fLogFile) then
      fsOut:= TFileStream.Create(fLogFile, fmOpenWrite)
    else
      fsOut:= TFileStream.Create(fLogFile, fmCreate);

    fsOut.Position:= fsOut.Size;
    fsOut.Write(aPreparedString[1], length(aPreparedString));
    fsOut.Free;
  except
    if Assigned(fsOut) then fsOut.Free;
  end;
end;

procedure TServiceGuard.SetLogMode(aLogMode: TLogMode);
begin
  case aLogMode of
    lmOff: SetLength(fLogMode,0);
    lmError:
      begin
        SetLength(fLogMode,1);
        fLogMode[0]:= ltError;
      end;
    lmSystem:
      begin
        SetLength(fLogMode,2);
        fLogMode[0]:= ltError;
        fLogMode[1]:= ltSystem;
      end;
    lmDebug:
      begin
        SetLength(fLogMode,3);
        fLogMode[0]:= ltError;
        fLogMode[1]:= ltSystem;
        fLogMode[2]:= ltDebug;
      end;
    lmDevelop:
      begin
        SetLength(fLogMode,4);
        fLogMode[0]:= ltError;
        fLogMode[1]:= ltSystem;
        fLogMode[2]:= ltDebug;
        fLogMode[3]:= ltDevelop;
      end;
  end;
end;

function TServiceGuard.LogTypeAsString(aLogType: TLogType):string;
begin
  Result:= '';

  case aLogType of
    ltSystem    : Result:= 'System';
    ltError     : Result:= 'Error';
    ltDebug     : Result:= 'Debug';
    ltDevelop   : Result:= 'Develop';
  end;
end;

function TServiceGuard.CheckLogType(aLogType: TLogType):boolean;
var
  i: Integer;
begin
  Result:= false;

  if Assigned(fLogMode) then
    for i:=0 to High(fLogMode) do
      if fLogMode[i] = aLogType then
        begin
          Result:= true;
          Exit;
        end;
end;

function TServiceGuard.GetQueue: TRunMode;
begin
  Result:= QueuePop;
end;

function TServiceGuard.GetQueueIsEmpty: boolean;
begin
  Result:= fQueue.Size=0;
end;

procedure TServiceGuard.WriteLog(aText: string; const aLogType: TLogType; const WriteTime: boolean);
begin
 if not CheckLogType(aLogType) then exit;
   if UTF8Length(aText)>0 then
     aText:= '['+LogTypeAsString(aLogType)+'] '+ aText;

   WriteLogInFile(aText, WriteTime);
end;

end.

