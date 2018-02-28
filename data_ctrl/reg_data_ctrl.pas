unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, obj_proto, dictionary;

{ Функция создания объекта контроллера данных по имени типа}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary=nil): TICObjectProto;

implementation

uses
    log, remoute_opc_node, opc_server_node;
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
    if oParent <> nil then
        result.SetParent(oParent);
    if Properties <> nil then
        result.SetProperties(Properties);
    exit;
  end;

  WarningMsg(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  result := nil;
end;

{
Функция создания объекта контроллера данных по имени типа.
ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param (sTypeName Наименование типа. Прописывается в INI файле
       в секции контроллера данных параметр 'type')
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;
begin
  if sTypeName = 'OPC_SERVER_NODE' then
  begin
    Result := TICOPCServerNode.Create;
    if oParent <> nil then
        Result.SetParent(oParent);
    if Length(aArgs) >= 1 then
    begin
        { Первый элемент - это имя OPC сервера }
        Result.SetOPCServerName(aArgs[0]);
    end;

    exit;
  end;

  WarningMsg(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  result := nil;
end;

end.

