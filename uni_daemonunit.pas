{
Модуль обработчиков демона/службы
}
unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,
  Interfaces; { this includes the LCL widgetset }

type

  { TUniReaderDaemon  непосредственно экземпляр сервиса.
  Именно этот объект реализует сам сервис (его мы видим в Windows в «Управление компьютерами/Сервисы»).}
  TUniReaderDaemon = class(TDaemon)
    { Обработчик после инсталяции }
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    { Обработчик после деинсталяции }
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    { Обработчик запуска демона/службы }
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    { Обработчик останова демона/службы }
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private

  public

  end;

var
  UniReaderDaemon: TUniReaderDaemon;

implementation

uses
  log, engine;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TUniReaderDaemon)
end;

{$R *.lfm}

{ TUniReaderDaemon }

procedure TUniReaderDaemon.DataModuleAfterInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is installing');
end;

procedure TUniReaderDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is deinstalling');
end;

procedure TUniReaderDaemon.DataModuleCreate(Sender: TObject);
begin
end;

procedure TUniReaderDaemon.DataModuleDestroy(Sender: TObject);
begin
end;

procedure TUniReaderDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);

begin
  engine.READER_ENGINE := TICReader.Create(nil);
  //engine.READER_ENGINE.RegRpcMethods;
  engine.READER_ENGINE.StartServer;
end;

procedure TUniReaderDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  engine.READER_ENGINE.StopServer;
  engine.READER_ENGINE.Destroy;
  engine.READER_ENGINE := nil;
end;


initialization
  RegisterDaemon;
end.

