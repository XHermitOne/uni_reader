Program uni_reader;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp,  EventLog, SysUtils,
  lazdaemonapp, uni_daemonmapperunit, uni_daemonunit
  { add your units here };

begin
  //Application.Initialize;
  //// Application.Logger := TAppLogger.Create;
  //Application.Run;
  with Application do
   begin
     Title := 'UniReaderGateway Daemon';
     { Указываем режим журналирования в log файл }
     EventLog.LogType := ltFile;
     EventLog.DefaultEventType := etDebug;
     EventLog.AppendContent := true;
     { Имя файла журнала }
     EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');

     Initialize;
     Run;
   end;
end.
