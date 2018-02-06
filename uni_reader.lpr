program uni_reader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  dictionary, sysfunc, strfunc, filefunc, config, log,
  settings, inifunc,
  obj_proto, reg_data_ctrl, keyboardfunc, engine,
  remoute_opc_node, memfunc
  { you can add units after this };

type

  { TICUniReaderApplication }

  TICUniReaderApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TICUniReaderApplication }

procedure TICUniReaderApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  InitStatusMemory();

  OpenLog();

  ENVIRONMENT.PrintContent;

  READER_ENGINE.Run('diagnostic');
  READER_ENGINE.Free;

  CloseLog();
  // ENVIRONMENT.Clear();
  PrintLostMemory();

  // stop program loop
  Terminate;
end;

constructor TICUniReaderApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TICUniReaderApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TICUniReaderApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TICUniReaderApplication;
begin
  Application:=TICUniReaderApplication.Create(nil);
  Application.Title:='UniReader';
  Application.Run;
  Application.Free;
end.

