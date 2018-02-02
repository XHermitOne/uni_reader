unit remoute_opc_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, obj_proto;

type
  {
  Класс взаимодействия с удаленным OPC сервером.
  }
  TICRemouteOPCNode = class(TICObjectProto)
  public
    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; override;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; override;
    { Функция диагностики контроллера данных }
    function Diagnostic(): Boolean; override;
end;

implementation

{
Фунция чтения данных
}
function TICRemouteOPCNode.Read(aValues: TStringList): TStringList;
begin
  result := Nil;
end;

{
Фунция записи данных
}
function TICRemouteOPCNode.Write(aValues: TStringList): Boolean;
begin
  result := False;
end;

{
Функция диагностики контроллера данных
}
function TICRemouteOPCNode.Diagnostic(): Boolean;
begin
  result := False;
end;

end.

