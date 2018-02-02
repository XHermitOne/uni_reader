unit config;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary;

type

    {
    TICEnvironment - Менеджер окружения программы
    }

    TICEnvironment = class(TStrDictionary)
    end;

var
  {
  Объявление глобального объекта окружения

  ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
  Переменные определенные в секции implementation являются статическими для
  модуля.
  }
  ENVIRONMENT: TICEnvironment;

implementation

uses
    filefunc, log;

var
  log_filename: AnsiString;

begin
  ENVIRONMENT := TICEnvironment.Create;
  ENVIRONMENT.AddObject('DEBUG_MODE', Nil);

  log_filename := JoinPath([GetHomeDir(), '.uni_reader',
                            Format('uni_reader_%s.log', [FormatDateTime('YYYY_MM_DD', Now)])]);
  service(log_filename);
  ENVIRONMENT.AddStrValue('LOG_FILENAME', log_filename);

  ENVIRONMENT.AddObject('LOG_MODE', Nil);

  // ENVIRONMENT.PrintContent;
  // writeln(ENVIRONMENT.IsVariable('DEBUG_MODE'));
  // writeln(ENVIRONMENT.IsVariable('LOG_MODE'));
end.

