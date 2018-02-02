unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, log, obj_proto, remoute_opc_node;

{ Функция создания объекта контроллера данных по имени типа}
function CreateRegDataCtrl(sTypeName: AnsiString): TicObjectProto;

implementation

{
Функция создания объекта контроллера данных по имени типа.
ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param (sTypeName Наименование типа. Прописывается в INI файле
       в секции контроллера данных параметр 'type')
}
function CreateRegDataCtrl(sTypeName: AnsiString): TicObjectProto;
begin
  if sTypeName = 'REMOUTE_OPC_NODE' then
  begin
    result := TicRemouteOPCNode.Create;
    exit;
  end;

  warning(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  result := Nil;
end;

end.

