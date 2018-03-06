Program uni_reader;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp,  EventLog, SysUtils,
  lazdaemonapp, uni_daemonmapperunit, uni_daemonunit,
  engine, log
  { add your units here };

begin
  Application.Title:='Daemon application';
  { Чтение параметров коммандной строки }
  if Application.HasOption('p', 'port') then
    try
      { Если указан не  стандартный порт, то запоминаем его в переменной }
      engine.XML_RPC_PORT := StrToInt(Application.GetOptionValue('p', 'port'));
    except
      log.FatalMsg('Ошибка параметра коммандной строки. Порт XML RPC');
    end;

  // Запуск по умолчанию
  // vvvvvvvvvvvvvvvvvvvvvvv
  // Application.Initialize;
  // Application.Run;
  // ^^^^^^^^^^^^^^^^^^^^^^^^
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
