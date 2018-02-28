unit opc_server_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    obj_proto, dictionary, strfunc,
    opc_client, tag_list;

const
  RESERV_PROPERTIES: Array [1..6] Of String = ('type', 'name', 'description', 'opc_server');

type
  {
  Класс взаимодействия с OPC сервером.
  }
  TICOPCServerNode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    FOPCClient: TOPCClient;

    { Наименование OPC сервера }
    FOPCServerName: AnsiString;

  public
    constructor Create;
    procedure Free;

    { Установить наименование OPC сервера }
    procedure SetOPCServerName(sName: AnsiString);

    //{ Выбрать описания тегов из свойств }
    //function CreateTags(): TStrDictionary;

    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; override;
    function Read(aValues: Array Of Const): TStringList; override;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; override;
    { Функция диагностики контроллера данных }
    function Diagnostic(): Boolean; override;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log;

constructor TICOPCServerNode.Create;
begin
     inherited Create;
     FOPCClient := nil;
end;

procedure TICRemouteOPCNode.Free;
begin
  if FOPCClient <> nil then
     FOPCClient.Destroy;
  inherited Free;
end;

{ Установить наименование OPC сервера }
procedure TICRemouteOPCNode.SetOPCServerName(sName: AnsiString);
begin
  FOPCServerName := sName;
end;

{
Фунция чтения данных
}
function TICRemouteOPCNode.Read(aValues: TStringList): TStringList;
begin
  Result := nil;
end;

function TICRemouteOPCNode.Read(aValues: Array Of Const): TStringList;
begin
  Result := nil;
end;

{
Фунция записи данных
}
function TICRemouteOPCNode.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{
Функция диагностики контроллера данных
}
function TICRemouteOPCNode.Diagnostic(): Boolean;
//var
//  i, j: Integer;
//  tags: TStrDictionary;
//  grp: TGroup;
//  tag_item: TTagItem;
//  start_dt, stop_dt: DWORD;
begin
  //log.InfoMsg(Format('Диагностика объекта <%s> класса <%s>', [GetName(), ClassName]));
  //
  //FOPCClient := TOPCClient.Create(nil);
  //// ВНИМАНИЕ! В Lazarus необходимо указывать @ для связки события с обработчиком
  ////                        V
  //// FOPCClient.OnChangeTag := @OPCClientChangeTag;
  //
  //FOPCClient.ServerName := 'RSLinx OPC Server';
  //
  //tags := CreateTags;
  //log.DebugMsg(Format('Создание группы <%s>', [GetName()]));
  //
  //grp := TGroup.Create(GetName(), 500, 0);
  //for i := 0 to tags.Count - 1 do
  //begin
  //  log.ServiceMsg(Format('Добавление тега в OPC клиент <%s> : <%s>', [tags.GetKey(i), tags.GetStrValue(tags.GetKey(i))]));
  //  tag_item := TTagItem.Create(tags.GetKey(i), tags.GetStrValue(tags.GetKey(i)), VT_BSTR, acRead);
  //  grp.AddTag(tag_item);
  //end;
  //FOPCClient.TagList.AddGroup(grp);
  //
  //FOPCClient.Connect;
  //
  //// Чтение списка тегов
  //for j := 0 to 10 do
  //begin
  //  start_dt :=  GetTickCount();
  //  for i := 0 to tags.Count - 1 do
  //  begin
  //    log.DebugMsgFmt('Tag [%s] Value [%s]', [tags.GetKey(i),
  //                    FOPCClient.GetTagString(FOPCClient.FindSGroupSTag('RSLINX_01700_1',
  //                    tags.GetKey(i)))]);
  //  end;
  //  stop_dt :=  GetTickCount() - start_dt;
  //  log.DebugMsgFmt('Время выполнения %d мсек', [stop_dt]);
  //  Sleep(5000); //Задержка в 5 сек
  //end;
  //
  //log.DebugMsg('---');
  //FOPCClient.Disconnect;
  //
  //tags.Destroy;
  //result := False;
end;

//{ Выбрать описания тегов из свойств }
//function TICRemouteOPCNode.CreateTags(): TStrDictionary;
//var
//  i: Integer;
//  key, value: AnsiString;
//  tags: TStrDictionary;
//begin
//  tags := TStrDictionary.Create;
//  for i := 0 to Properties.Count - 1 do
//  begin
//    key := Properties.GetKey(i);
//    if not IsStrInList(key, RESERV_PROPERTIES) then
//    begin
//      value := Properties.GetStrValue(key);
//      tags.AddStrValue(key, value);
//    end;
//  end;
//  result := tags;
//end;

end.

