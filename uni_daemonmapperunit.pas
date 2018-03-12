{
Модуль маппера демона/службы
}
unit uni_daemonmapperunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,
  engine, log;

type

  { TUniReaderDaemonMapper  — это компонент, отвечающий за управление работой сервисов.
  Особенно актуален он в случае включения в один исполняемый модуль нескольких сервисов.}
  TUniReaderDaemonMapper = class(TDaemonMapper)
    {
    Обработчик выполнения инсталяции демона/службы

    ВНИМАНИЕ! Здесь добавляем ключи для запуска прослушки не стандартного порта
    Прослушиваемый порт указывается при инсталляции службы:

    uni_reader.exe --port=8081 --install

    с помощью RunArguments этот ключ переносится в комманду запуска службы:

    uni_reader.exe --run --port=8081
    }
    procedure UniReaderDaemonMapperInstall(Sender: TObject);
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

{ TUniReaderDaemonMapper }

procedure TUniReaderDaemonMapper.UniReaderDaemonMapperInstall(Sender: TObject);
begin
  { ВНИМАНИЕ! Здесь добавляем ключи для запуска прослушки не стандартного порта
  Прослушиваемый порт указывается при инсталляции службы:
  uni_reader.exe --port=8081 --install
  с помощью RunArguments этот ключ переносится в комманду запуска службы:
  uni_reader.exe --run --port=8081}
  if engine.XML_RPC_PORT <> DEFAULT_XML_RPC_PORT then
    DaemonDefs[0].RunArguments := Format('--port=%d', [engine.XML_RPC_PORT]);
  log.DebugMsgFmt('Run arguments <%s>', [DaemonDefs[0].RunArguments]);
end;


initialization
  RegisterMapper;
end.

