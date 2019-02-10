unit wWinServices;

{$mode objfpc}{$H+}

{
  Class for working with Windows services.
  Based on http://www.freepascal.ru/forum/viewtopic.php?f=13&t=4995#p39184

  wofs(c)2019 [wofssirius@yandex.ru]
  GNU LESSER GENERAL PUBLIC LICENSE v.2.1
}
interface

uses Windows, Classes, SysUtils, jwaWinNT, jwaWinSvc, gvector;

type

  TwService = record
    Name: string;
    Status: word;
    StatusText: string;
  end;

  TwServices = specialize TVector<TwService>;

  { TWinServices }

  TWinServices = class
  private
    function CreateServiceItem(aName: string; aStatus: word; aStatusText: string): TwService;
    function GetComputerName_: string;
    function GetServicesList: TwServices;
    function GetUserCurrentName: string;
    function ServiceStateText(State: word): string;

  protected

  const
    SC_NotFoundString      = 'Service not found';
    SC_StoppedString       = 'Stopped';
    SC_Start_PendingString = 'Start Pending';
    SC_Stop_PendingString  = 'Stop Pending';
    SC_RunningString       = 'Running';
    SC_PausedString        = 'Paused';
    SC_1722_String         = 'The RPC server is unavailable';
    SC_StatusString        = 'Status: ';

  public
    constructor Create;
    destructor Destroy; override;

    function ServiceStop(aMachine, aServiceName: string): boolean;
    function ServiceStart(aMachine, aServiceName: string): boolean;
    function ServiceGetStatus(aMachine, aService: string): DWord;
    function ServiceGetStatusText(aMachine, aService: string): string;

    function ServiceSetMode(aMachine, aService: string; Mode: word): boolean;

    property ServicesList: TwServices read GetServicesList;
    property ComputerName:string read GetComputerName_;
    property UserCurrentName:string read GetUserCurrentName;
  end;


const
  SC_NotFound = 0;
  SC_Stopped = 1; { the status of the services }
  SC_Start_Pending = 2;
  SC_Stop_Pending = 3;
  SC_Running = 4;
  SC_Paused = 7;

implementation

{ TWinServices }

function TWinServices.CreateServiceItem(aName: string; aStatus: word;
  aStatusText: string): TwService;
begin
  Result.Name := aName;
  Result.Status := aStatus;
  Result.StatusText:= aStatusText;
end;

function TWinServices.GetComputerName_: string;
var
  aName: array[0..20] of char;
  aNameSize: DWORD;
begin
  aNameSize:= SizeOf(aName);

  GetComputerName(@aName, aNameSize);

  Result:= aName;
end;

constructor TWinServices.Create;
begin
  inherited Create;
end;

destructor TWinServices.Destroy;
begin
  inherited Destroy;
end;

function TWinServices.ServiceStop(aMachine, aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  dwCheckPoint: DWord;
begin
  h_manager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager, PChar(aServiceName), SERVICE_STOP or
      SERVICE_QUERY_STATUS);

    if h_svc > 0 then
    begin
      if (ControlService(h_svc, SERVICE_CONTROL_STOP, svc_status)) then
      begin
        if (QueryServiceStatus(h_svc, svc_status)) then
        begin
          while (SERVICE_STOPPED <> svc_status.dwCurrentState) do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);

            if (not QueryServiceStatus(h_svc, svc_status)) then
            begin
              // couldn't check status
              break;
            end;

            if (svc_status.dwCheckPoint < dwCheckPoint) then
              break;

          end;
        end;
      end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := SERVICE_STOPPED = svc_status.dwCurrentState;
end;

function TWinServices.ServiceStart(aMachine, aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  Temp: PChar;
  dwCheckPoint: DWord;
begin
  svc_status.dwCurrentState := SC_Stopped;
  h_manager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager, PChar(aServiceName), SERVICE_START or
      SERVICE_QUERY_STATUS);
    if h_svc > 0 then
    begin
      temp := nil;
      if (StartService(h_svc, 0, temp)) then
        if (QueryServiceStatus(h_svc, svc_status)) then
        begin
          while (SERVICE_RUNNING <> svc_status.dwCurrentState) do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;

            Sleep(svc_status.dwWaitHint);

            if (not QueryServiceStatus(h_svc, svc_status)) then
              break;

            if (svc_status.dwCheckPoint < dwCheckPoint) then
            begin
              // QueryServiceStatus not to increase dwCheckPoint
              break;
            end;
          end;
        end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;
  Result := SERVICE_RUNNING = svc_status.dwCurrentState;
end;

function TWinServices.ServiceGetStatus(aMachine, aService: string): DWord;
var
  h_manager, h_svc: SC_Handle;
  service_status: TServiceStatus;
  hStat: DWord;
begin
  hStat := SC_NotFound;
  h_manager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);

  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager, PChar(aService), SERVICE_QUERY_STATUS);

    if h_svc > 0 then
    begin
      if (QueryServiceStatus(h_svc, service_status)) then
        hStat := service_status.dwCurrentState;

      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := hStat;
end;

function TWinServices.ServiceGetStatusText(aMachine, aService: string
  ): string;
begin
  Result:= ServiceStateText(ServiceGetStatus(aMachine, aService));
end;

function TWinServices.ServiceSetMode(aMachine, aService: string; Mode: word
  ): boolean;
  // Service startup type: SERVICE_DEMAND_START
  //                      SERVICE_AUTO_START
var
  h_manager, h_svc: SC_Handle;
  SvcLock: SC_Lock;
begin
  Result := False;
  h_manager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_All_Access);

  if h_manager > 0 then
  begin
    // Block the database of services.
    SvcLock := LockServiceDatabase(h_manager);
    if SvcLock = nil then
      exit;// Service blocking error
    h_svc := OpenService(h_manager, PChar(aService), SERVICE_All_Access);

    // Let's check that we were able to access the service.
    if h_svc > 0 then
    begin
      if not ChangeServiceConfig(h_svc, SERVICE_NO_CHANGE, Mode,
        SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
        // Error changing service status.
        exit;

    end;
    UnlockServiceDatabase(SvcLock);
    CloseServiceHandle(h_svc);
    CloseServiceHandle(h_manager);

  end
  else
    exit;
  //Exit by mistake access to services.

  Result := True;
end;

function TWinServices.ServiceStateText(State: word): string;
begin
  case State of
    SC_NotFound        : Result:= SC_NotFoundString;
    SC_Stopped         : Result:= SC_StoppedString;
    SC_Start_Pending   : Result:= SC_Start_PendingString;
    SC_Stop_Pending    : Result:= SC_Stop_PendingString;
    SC_Running         : Result:= SC_RunningString;
    SC_Paused          : Result:= SC_PausedString;
    1722               : Result:= SC_1722_String;
    else
      Result           := SC_StatusString + IntToStr(State)
  end; { case }
end;

function TWinServices.GetServicesList: TwServices;
const
  BuffSize = SizeOf(ENUM_SERVICE_STATUS) * 4096;

var
  Status: PENUMSERVICESTATUS;
  Man: SC_HANDLE;
  j: integer;
  N: DWord = 0;
  R: DWord = 0;
  H: DWord = 0;
begin
  Result:= TwServices.Create;

  Result.Clear;

  Man := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);

  if Man <> 0 then
  begin
    Status := GetMem(BuffSize);
    if EnumServicesStatus(Man, SERVICE_WIN32, SERVICE_STATE_ALL, Status, BuffSize, N, R, H) then
    begin
      for j := 0 to R - 1 do
        Result.PushBack(CreateServiceItem(Status[j].lpServiceName, Status[j].ServiceStatus.dwCurrentState, ServiceStateText(Status[j].ServiceStatus.dwCurrentState)));
    end;
    Freemem(Status);
    CloseServiceHandle(Man);
  end;
end;

function TWinServices.GetUserCurrentName: string;
var
  aName: array[0..30] of char;
  aNameSize: DWORD;
begin
  aNameSize:= SizeOf(aName);

  GetUserName(@aName, aNameSize);

  Result:= aName;
end;

begin
end.
