unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,
  Interfaces; { this includes the LCL widgetset }

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

uses
  log, filefunc, engine;

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
//var
//  log_filename: AnsiString;

begin
  //InitStatusMemory();

  // ENVIRONMENT.PrintContent;
  //log_filename := filefunc.JoinPath([filefunc.GetHomeDir(), '.uni_reader',
  //                                   Format('uni_reader_%s.log', [FormatDateTime('YYYY_MM_DD', Now)])]);
  //log_filename := 'c:\tmp\debug.log';
  //Application.FileName := log_filename;

  // log.OpenLog(log_filename);
  //Application.Log(etDebug, '>>> ' + Name);
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
  //Application.Log(etDebug, '<<< ' + Name);
  //log.CloseLog();
  // WriteLn('The ' + Name + ' service is not running');
  // PrintLostMemory();
  //config.ENVIRONMENT.Free;
  //config.ENVIRONMENT := nil;
end;


initialization
  RegisterDaemon;
end.

