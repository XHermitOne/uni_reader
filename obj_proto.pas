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
    Parent: TObject;
    { Наменование объекта }
    Name: AnsiString;
    { Описание объекта }
    Description: AnsiString;
    { Имена читаемых значений из контроллера данных }
    ReadValues: TStringList;
    { ВНИМАНИЕ! Источники данных запоминают после чтения состояние переменных для
     последующего доступа к ним объектов приемников данных
     Вот это словарь переменных }
    State: TStrDictionary;

    { Свойства контроллера данных. Прописаны в INI файле }
    Properties: TStrDictionary;

  public
    constructor Create;
    destructor Destroy; override;

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

constructor TICObjectProto.Create;
begin
     inherited Create;
     Parent := Nil;
     Name := 'Unknown';
     Description := '';
     ReadValues := TStringList.Create;
     State := TStrDictionary.Create;
end;

destructor TICObjectProto.Destroy;
begin
     ReadValues.Free;
     State.Free;
     inherited Destroy;
end;

function TICObjectProto.GetName(): AnsiString;
begin
     result := Name;
end;

procedure TICObjectProto.SetName(sName: AnsiString);
begin
     Name := sName;
end;

function TICObjectProto.GetParent(): TObject;
begin
     result := Parent;
end;

procedure TICObjectProto.SetParent(oParent: TObject);
begin
     Parent := oParent;
end;

function TICObjectProto.GetProperties(): TStrDictionary;
begin
     result := Properties;
end;

procedure TICObjectProto.SetProperties(dProperties: TStrDictionary);
begin
     Properties := dProperties;
end;

{
Проверка на то что объект не именованный.
}
function TICObjectProto.IsUnknown(): Boolean;
begin
     result := Name = 'Unknown';
end;

{
Зарегистрировать значения переменных в словаре внутренного состояния.
@param (Values Словарь переменных)
}
function TICObjectProto.RegState(aValues: TStrDictionary): Boolean;
begin
    result := State.Update(aValues);
end;

{
Получить имена записываемых значений в контроллер данных
}
function TICObjectProto.GetReadValues(): TStringList;
begin
     result := ReadValues;
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

