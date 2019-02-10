Program project1;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonMapperU, DaemonU, vFunc
  { add your units here };

{$R *.res}

begin
  Application.Title:='Service.Guard';
  Application.Initialize;
  Application.Run;
end.
