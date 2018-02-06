unit obj_proto;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary;

type
  {
  Абстрактный объект системы.
  Реализует общие функции для всех объектов.
  }
  TICObjectProto = class(TObject)
  private
    { Объект родительского управляющего объекта }
    FParent: TObject;
    { Наменование объекта }
    FName: AnsiString;
    { Описание объекта }
    FDescription: AnsiString;
    { Имена читаемых значений из контроллера данных }
    FReadValues: TStringList;
    { ВНИМАНИЕ! Источники данных запоминают после чтения состояние переменных для
     последующего доступа к ним объектов приемников данных
     Вот это словарь переменных }
    FState: TStrDictionary;

    { Свойства контроллера данных. Прописаны в INI файле }
    FProperties: TStrDictionary;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    function GetName(): AnsiString;
    procedure SetName(sName: AnsiString);

    function GetParent(): TObject;
    procedure SetParent(oParent: TObject);

    function GetProperties(): TStrDictionary;
    procedure SetProperties(dProperties: TStrDictionary);

    { Проверка на то что объект не именованный }
    function IsUnknown(): Boolean;
    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; virtual;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; virtual;
    { Зарегистрировать значения переменных в словаре внутренного состояния }
    function RegState(aValues: TStrDictionary): Boolean;
    { Получить имена записываемых значений в контроллер данных }
    function GetReadValues(): TStringList;
    { Функция диагностики контроллера данных }
    function Diagnostic(): Boolean; virtual;
end;


implementation

uses
    log;

constructor TICObjectProto.Create;
begin
     inherited Create;
     FParent := Nil;
     FName := 'Unknown';
     FDescription := '';
     FReadValues := TStringList.Create;
     FState := TStrDictionary.Create;
end;

destructor TICObjectProto.Destroy;
begin
     FReadValues.Free;
     FState.Free;
     inherited Destroy;
end;

procedure TICObjectProto.Free;
begin
  FReadValues.Free;
  FState.Free;
  FProperties.Free;
  inherited Free;
end;

function TICObjectProto.GetName(): AnsiString;
begin
     result := FName;
end;

procedure TICObjectProto.SetName(sName: AnsiString);
begin
     FName := sName;
end;

function TICObjectProto.GetParent(): TObject;
begin
     result := FParent;
end;

procedure TICObjectProto.SetParent(oParent: TObject);
begin
     FParent := oParent;
end;

function TICObjectProto.GetProperties(): TStrDictionary;
begin
     result := FProperties;
end;

procedure TICObjectProto.SetProperties(dProperties: TStrDictionary);
begin
     FProperties := dProperties;
     if FProperties.HasKey('name') then
        SetName(FProperties.GetStrValue('name'))
     else
        WarningMsg(Format('Не определено имя объекта в свойствах. Класс <%s>', [ClassName]));
end;

{
Проверка на то что объект не именованный.
}
function TICObjectProto.IsUnknown(): Boolean;
begin
     result := FName = 'Unknown';
end;

{
Зарегистрировать значения переменных в словаре внутренного состояния.
@param (Values Словарь переменных)
}
function TICObjectProto.RegState(aValues: TStrDictionary): Boolean;
begin
    result := FState.Update(aValues);
end;

{
Получить имена записываемых значений в контроллер данных
}
function TICObjectProto.GetReadValues(): TStringList;
begin
     result := FReadValues;
end;

{
Фунция чтения данных
}
function TICObjectProto.Read(aValues: TStringList): TStringList;
begin
  result := Nil;
end;

{
Фунция записи данных
}
function TICObjectProto.Write(aValues: TStringList): Boolean;
begin
  result := False;
end;

{
Функция диагностики контроллера данных
}
function TICObjectProto.Diagnostic(): Boolean;
begin
  result := False;
end;

end.

