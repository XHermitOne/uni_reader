unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, obj_proto, dictionary;

{ Функция создания объекта контроллера данных по имени типа}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary=Nil): TICObjectProto;

implementation

uses
    log, remoute_opc_node;
{
Функция создания объекта контроллера данных по имени типа.
ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param (sTypeName Наименование типа. Прописывается в INI файле
       в секции контроллера данных параметр 'type')
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary): TICObjectProto;
begin
  if sTypeName = 'REMOUTE_OPC_NODE' then
  begin
    result := TICRemouteOPCNode.Create;
    if oParent <> Nil then
        result.SetParent(oParent);
    if Properties <> Nil then
        result.SetProperties(Properties);
    exit;
  end;

  WarningMsg(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  result := Nil;
end;

end.

