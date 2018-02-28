unit uni_daemonmapperunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type
  TUniReaderDaemonMapper = class(TDaemonMapper)
  private

  public

  end;

var
  UniReaderDaemonMapper: TUniReaderDaemonMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TUniReaderDaemonMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

