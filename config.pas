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
    {
    ВНИМАНИЕ! В этом модуле нельзя использовать модуль log т.к. происходит
    взаимное использование модулей и выполнение программы сваливается
    в <Segmentation fault>.
    }
    filefunc, memfunc;

var
  log_filename: AnsiString;

begin
  //InitStatusMemory();
  ENVIRONMENT := TICEnvironment.Create;

  log_filename := JoinPath([GetHomeDir(), '.uni_reader',
                            Format('uni_reader_%s.log', [FormatDateTime('YYYY_MM_DD', Now)])]);
  ENVIRONMENT.AddStrValue('LOG_FILENAME', log_filename);

  ENVIRONMENT.AddObject('LOG_MODE', Nil);
  ENVIRONMENT.AddObject('DEBUG_MODE', Nil);
  //PrintLostMemory();

  // ENVIRONMENT.PrintContent;
  // writeln(ENVIRONMENT.IsVariable('DEBUG_MODE'));
  // writeln(ENVIRONMENT.IsVariable('LOG_MODE'));
end.

