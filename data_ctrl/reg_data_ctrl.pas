{
Функции регистрации объектов источников данных

Версия: 0.0.3.3
}
unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  obj_proto, dictionary;

{
Функция создания объекта контроллера данных по имени типа

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param oParent Родительский объект
@param sTypeName Наименование типа источника/контроллера данных. Прописывается в INI файле в секции контроллера данных параметр 'type'
@param Properties Словарь свойств
@return Созданный объект. Необходимо для использования сделать преобразование типа
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary=nil): TICObjectProto;

{
Функция создания объекта контроллера данных по имени типа

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param oParent Родительский объект
@param sTypeName Наименование типа источника/контроллера данных. Прописывается в INI файле в секции контроллера данных параметр 'type'
@param Args Массив свойств
@return Созданный объект. Необходимо для использования сделать преобразование типа
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;

implementation

uses
  log,
  // Компоненты - источники данных
  opc_da_node, opc_hda_node, opc_wt_hda_node;

{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary): TICObjectProto;
begin
  if sTypeName = opc_da_node.OPC_DA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_da_node.TICOPCDANode.Create;
  end
  else if sTypeName = opc_hda_node.OPC_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_hda_node.TICOPCHDANode.Create;
  end
  else if sTypeName = opc_wt_hda_node.OPC_WT_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_wt_hda_node.TICWtOPCHDANode.Create;
  end
  else
  begin
    log.WarningMsgFmt('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]);
    Result := nil;
  end;

  if Result <> nil then
  begin
    if oParent <> nil then
      Result.SetParent(oParent);
    if Properties <> nil then
      Result.SetProperties(Properties);
  end;
end;

{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;
begin
  if sTypeName = opc_da_node.OPC_DA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_da_node.TICOPCDANode.Create;
  end
  else if sTypeName = opc_hda_node.OPC_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC HDA сервера }
    Result := opc_hda_node.TICOPCHDANode.Create;
  end
  else if sTypeName = opc_wt_hda_node.OPC_WT_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC HDA сервера }
    Result := opc_wt_hda_node.TICWtOPCHDANode.Create;
  end
  else
    Result := nil;

  if Result = nil then
  begin
    log.WarningMsgFmt('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]);
  end
  else
  begin
    if oParent <> nil then
        Result.SetParent(oParent);
    Result.SetPropertiesArray(aArgs);
  end;
end;

end.

