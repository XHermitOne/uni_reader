unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,
  Interfaces, { this includes the LCL widgetset }
  engine, config, log;
  //dictionary, sysfunc, config, log,
  //settings, engine, remoute_opc_node;

type

  { TUniReaderDaemon }

  TUniReaderDaemon = class(TDaemon)
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private

  public

  end;

var
  UniReaderDaemon: TUniReaderDaemon;

implementation

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
  //InitStatusMemory();

  // ENVIRONMENT.PrintContent;

  READER_ENGINE := TICReader.Create(nil);
  READER_ENGINE.RegRpcMethods;
  //READER_ENGINE.Run('diagnostic');

  //CloseLog();
  // ENVIRONMENT.Clear();
  // PrintLostMemory();

  // WriteLn('The ' + Name + ' service is running');
end;

procedure TUniReaderDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  READER_ENGINE.Free;
  //log.CloseLog();
  // WriteLn('The ' + Name + ' service is not running');
  // PrintLostMemory();
  //config.ENVIRONMENT.Free;
  //config.ENVIRONMENT := nil;
end;


initialization
  RegisterDaemon;
end.

