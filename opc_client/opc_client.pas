{ OPCClient - это компонент. Предназначен для написания систем сбора и управления
  данными, других программ, или превращения Lazarus в программу для создания
  SCADA систем.

  Copyright (C) 2013 Чигрин В.Н. vchigrin@mail.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit opc_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LResources, Forms, Controls, Graphics, Dialogs,
  tag_list,
  OPCDA, ActiveX;

resourcestring
  msgNoOPCServer  = 'Имя OPC сервера незадано!';
  msgNoConnectOPC = 'Невозможно соединиться с OPC сервером!';
  msgNoListTag    = 'Нет списка OPC-тэгов!';
  msgNoListTag1   = 'Список OPC-тэгов пустой!';
  msgErrAddGroup  = 'Ошибка добавления группы %s';
  msgNoGrounName  = 'Нет Группы с именем %s в списке тэгов!';
  msgNoTagName    = 'Нет Тэга с именем %s в списке тэгов!';
  msgErrAddItem   = 'Невозможно добавить элементы в OPC-группу %s';
  msgErrAddTag    = 'Ошибка при добавлении элемента %s';
  msgErrCallback  = 'Ошибка при создании функции обратного вызова.' + {#13#10} LineEnding
                    + 'Для группы %s';

  msgNoFindConnectionPoint =
                    'Нет сервиса FindConnectionPoint в сервере OPC (IOPCDataCallback)';
  msgErrAdvise    = 'Ошибка вызова Advise' + {#13#10} LineEnding
                    + 'для группы %s';
  msgErrUnadvise  = 'Ошибка вызова Unadvise' + {#13#10} LineEnding
                    + 'Для группы %s';
  msgErrDelTag    = 'Ошибка при удалении OPC-группы %s';
  msgErrDelItem   = 'Ошибка при удалении элемента %s';
  msgErrDelGroup  = 'Ошибка при удалении группы %s';
  msgErrNoTag     = 'Неверно уназан номер Группы или Тэга!!!';
  msgErrRecOPCTag = 'Ошибка записи Тэга в сервер!';
  msgNoOPC        = 'Ни один OPC-сервер не установлен' + {#13#10} LineEnding
                     + ' В системе!';

type
  EOPCError = class (Exception);

type

  {Позиция OPC - тэга в FTagList}
  {NGroup - Номер группы, NTag - Немер тэга}
  {Номера могут быть от 0 до Count - 1.
   Если номер равен -1 это значит Группы или Тэга с таким именем нет в списке.}
  TTagPosition = record
    NGroup, NTag: Integer
  end;

  TArrayOfTagPosition = Array Of TTagPosition;

  { TOPCClient }

  TOPCClient = class(TComponent)
  private
    { Указывает состояниеи компонта Подключен к северу/Отключен }
    FActive           : Boolean;
    FServerName       : String;
    { Список всех тэгов, которые появятся в OPC сервере }
    FTagList          : TTagList;
    m_pIOPCServer     : IOPCServer;
    { Изменить имя сервера }
    procedure SetServerName(AValue: String);
    { Процедуры чтения и сохранения списко тэгов в LFM }
    procedure ReadListTag(Reader: TReader);
    procedure WriteListTag(Writer: TWriter);
    { Установить значение в OPC сервер }
    procedure SetOPC(const aTagPosition: TTagPosition; const Val: OleVariant);
    { Получить значение из OPC сервера }
    function GetOPC(const aTagPosition: TTagPosition): OleVariant;
    procedure AddGroups;
    procedure AddTags(const Group: TGroup);
    procedure DeleteGroups;
    procedure DeleteTags(const Group: TGroup);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Подключить клиент к серверу }
    procedure Connect;
    { Отключить клиент от сервера }
    procedure Disconnect;
    property TagList : TTagList read FTagList;
    property Active : Boolean read FActive {write SetActive};
    {Поиск номера Группы и Тэга, сначала надо найти группу и сохранить номер,
     т.к. поиск по имени занимает много времени, потом вызвать чтение и запись Тэга}
    function FindSGroup(const aName: String): Integer;
    function FindSTag(const nG: Integer; const aName: String): TTagPosition;
    function FindSGroupSTag(const GName, TName: String): TTagPosition;
    function GetTagQuality(const aTagPosition: TTagPosition): Word;

    {Записать/Прочитать Тэг в/из сервера}
    {VT_BOOL (True=-1, False=0) (VARIANT_BOOL)}
    function GetTagBoolean(const aTagPosition: TTagPosition): Boolean;
    procedure SetTagBoolean(const aTagPosition: TTagPosition; const Val: Boolean);
    procedure SetQuiqTagBoolean(const aTagPosition: TTagPosition; const Val: Boolean);
    {VT_I2 (2 byte signed int)(SmallInt)}
    function GetTagSmallInt(const aTagPosition: TTagPosition): SmallInt;
    procedure SetTagSmallInt(const aTagPosition: TTagPosition; const Val: SmallInt);
    procedure SetQuiqTagSmallInt(const aTagPosition: TTagPosition; const Val: SmallInt);
    {VT_I4 (4 byte signed int)(Longint)}
    {VT_INT (signed machine int)(Longint)}
    function GetTagLongint(const aTagPosition: TTagPosition): LongInt;
    procedure SetTagLongint(const aTagPosition: TTagPosition; const Val: LongInt);
    procedure SetQuiqTagLongint(const aTagPosition: TTagPosition; const Val: LongInt);
    {VT_I1 (signed char)(ShortInt)}
    function GetTagShortInt(const aTagPosition: TTagPosition): ShortInt;
    procedure SetTagShortInt(const aTagPosition: TTagPosition; const Val: ShortInt);
    procedure SetQuiqTagShortInt(const aTagPosition: TTagPosition; const Val: ShortInt);
    {VT_I8 (signed 64-bit int)(Int64)}
    function GetTagInt64(const aTagPosition: TTagPosition): Int64;
    procedure SetTagInt64(const aTagPosition: TTagPosition; const Val: Int64);
    procedure SetQuiqTagInt64(const aTagPosition: TTagPosition; const Val: Int64);
    {VT_UI1 (unsigned char)(Byte)}
    function GetTagByte(const aTagPosition: TTagPosition): Byte;
    procedure SetTagByte(const aTagPosition: TTagPosition; const Val: Byte);
    procedure SetQuiqTagByte(const aTagPosition: TTagPosition; const Val: Byte);
    {VT_UI2 (unsigned short)(Word)}
    function GetTagWord(const aTagPosition: TTagPosition): Word;
    procedure SetTagWord(const aTagPosition: TTagPosition; const Val: Word);
    procedure SetQuiqTagWord(const aTagPosition: TTagPosition; const Val: Word);
    {VT_UI4 (unsigned long)(LongWord)}
    {VT_UINT (unsigned machine int)(LongWord)}
    function GetTagLongWord(const aTagPosition: TTagPosition): LongWord;
    procedure SetTagLongWord(const aTagPosition: TTagPosition; const Val: LongWord);
    procedure SetQuiqTagLongWord(const aTagPosition: TTagPosition; const Val: LongWord);
    {VT_UI8 (unsigned 64-bit int)(QWord)}
    function GetTagQWord(const aTagPosition: TTagPosition): QWord;
    procedure SetTagQWord(const aTagPosition: TTagPosition; const Val: QWord);
    procedure SetQuiqTagQWord(const aTagPosition: TTagPosition; const Val: QWord);
    {VT_CY (Currency)}
         {(8 байт, с плавающей запятой и 4 знаками после запятой,
          диапазон от -922 337 203 685 477,5808 до 922 337 203 685 477,5807,
          сопроцсссорпый тип)}
    function GetTagCurrency(const aTagPosition: TTagPosition): Currency;
    procedure SetTagCurrency(const aTagPosition: TTagPosition; const Val: Currency);
    procedure SetQuiqTagCurrency(const aTagPosition: TTagPosition; const Val: Currency);
    {VT_R4 (4 byte real)(Single)}
    function GetTagSingle(const aTagPosition: TTagPosition): Single;
    procedure SetTagSingle(const aTagPosition: TTagPosition; const Val: Single);
    procedure SetQuiqTagSingle(const aTagPosition: TTagPosition; const Val: Single);
    {VT_R8 (8 byte real)(Double)}
    function GetTagDouble(const aTagPosition: TTagPosition): Double;
    procedure SetTagDouble(const aTagPosition: TTagPosition; const Val: Double);
    procedure SetQuiqTagDouble(const aTagPosition: TTagPosition; const Val: Double);
    {VT_DATE (date)(TOleDate)}
    function GetTagDateTime(const aTagPosition: TTagPosition): TDateTime;
    procedure SetTagDateTime(const aTagPosition: TTagPosition; const Val: TDateTime);
    procedure SetQuiqTagDateTime(const aTagPosition: TTagPosition; const Val: TDateTime);
    {VT_BSTR (OLE Automation string)(POleStr (WideString))
         (Строка переменной длины, для храпения каждого символа используется 2 байта)}
    {VT_LPSTR (null terminated string)(PChar) (Указатель на строку, 4 байта)}
    {VT_LPWSTR (wide null terminated string)(PWideChar)
         (Указатель па строку, в который для храпения каждого символа
          используют 2 байта. Размер — 4 байта)}
    function GetTagString(const aTagPosition: TTagPosition): String;
    procedure SetTagString(const aTagPosition: TTagPosition; const Val: String);
    procedure SetQuiqTagString(const aTagPosition: TTagPosition; const Val: String);

  published
    {Имя OPC - сервера}
    property ServerName : String read FServerName write SetServerName;
  end;

{ Для работы с позицией Тэга (TTagPosition) }
operator =  (TP1, TP2: TTagPosition) b:Boolean;
operator <> (TP1, TP2: TTagPosition) b:Boolean;

implementation

uses
  Windows, ComObj, OPCTypes,
  log;

{$R OPC.RES}

operator = (TP1, TP2: TTagPosition)b: Boolean;
begin
  b := (TP1.NGroup = TP2.NGroup) and (TP1.NTag = TP2.NTag) and
       (TP1.NGroup <> -1) and (TP2.NGroup <> -1) and
       (TP2.NTag <> -1)   and (TP2.NTag <> -1);
end;

operator <> (TP1, TP2: TTagPosition)b: Boolean;
begin
  b := not (TP1 = TP2);
end;


{ TOPCClient }
constructor TOPCClient.Create(AOwner: TComponent);
begin
  // Application.Initialize;
  inherited Create(AOwner);
  // Создать объект для возврата данных
  FActive       := False;
  FServerName   := '';
  m_pIOPCServer := nil;
  FTagList      := TTagList.Create;
end;

destructor TOPCClient.Destroy;
begin
  Disconnect;
  m_pIOPCServer      := nil;
  FTagList.Free;
  inherited Destroy;
end;

{ Установить связь c OPC сервером }
procedure TOPCClient.Connect;
begin
  if ServerName = '' then
    log.WarningMsg(msgNoOPCServer)
  else
    if not FActive then
    begin
      try
        m_pIOPCServer := CreateComObject(ProgIDToClassID(ServerName)) as IOPCServer;
      except
        m_pIOPCServer := nil;
        log.FatalMsg(msgNoConnectOPC);
        Exit;
      end;
      AddGroups;
      FActive := True;
    end;
end;

{ Добавить группы в OPC сервер }
procedure TOPCClient.AddGroups;
var
  Group:   TGroup;
  myPercentDeadBand: Single;
  i:    Integer;
  HRes:    HRESULT;
begin
  // Добавляем группы
  if not Assigned(FTagList) then //raise EOPCError.Create(msgNoListTag);
  begin
    log.WarningMsg(msgNoListTag);
    Exit;
  end;

  if FTagList.Count < 1 then //raise EOPCError.Create(msgNoListTag1);
  begin
    log.WarningMsg(msgNoListTag1);
    Exit;
  end;

  // Пробегаем по группам
  for i := 0 to FTagList.Count - 1 do
  begin
    Group := FTagList[i];
    Group.hSGroup := i + 1;
    myPercentDeadBand := Group.PercentDeadBand;
    // Добавляем группу
    HRes  := m_pIOPCServer.AddGroup(PWideChar(WideString(Group.GroupName)),
        BOOL(True), Group.UpdateRate, Group.hSGroup, nil,
        @myPercentDeadBand, 0, Group.hSGroupCb, Group.UpdateRate,
        IOPCItemMgt, IUnknown(Group.m_pIOPCItemMgt));
    if Failed(HRes) then
    begin
      Group.m_pIOPCItemMgt := nil;
      Group.m_pIOPCSyncIO  := nil;
      log.WarningMsgFmt(msgErrAddGroup, [Group.GroupName]);
    end
    else
    begin
      Group.m_pIOPCSyncIO := Group.m_pIOPCItemMgt as IOPCSyncIO;
      AddTags(Group);
    end;
  end;
end;

procedure TOPCClient.AddTags(const Group: TGroup);
var
  j:    Integer;
  HRes:    HRESULT;
  ItemDef: array of OPCITEMDEF;
  Results: POPCITEMRESULTARRAY;
  Errors:  PResultList;
  //
  ItemIDs: array of WideString;
begin
  if Group.Count < 1 then
    Exit;
  // Добавляем OPC тэги в группу
  SetLength(ItemIDs, Group.Count);
  try
    for j := 0 to Group.Count - 1 do
      ItemIDs[j] := Group[j].TagPath;
    SetLength(ItemDef, Group.Count);
    try
      for j := 0 to Group.Count - 1 do
        with ItemDef[j] do
        begin
          szAccessPath := '';
          szItemID := PWideChar(ItemIDs[j]);
          bActive := BOOL(True);
          Group[j].hClient := j + 1;
          hClient := Group[j].hClient;
          dwBlobSize := 0;
          pBlob := nil;
          vtRequestedDataType := Group[j].Value.Typ;
        end;
      HRes := Group.m_pIOPCItemMgt.AddItems(Group.Count, @ItemDef[0], Results, Errors);
      if Failed(HRes) then
        log.WarningMsgFmt(msgErrAddItem, [Group.GroupName])
      else
        for j := 0 to Group.Count - 1 do
          if Failed(Errors^[j]) then
          begin
            log.WarningMsgFmt(msgErrAddTag, [Group[j].TagName]);
            Group[j].hClientCb := 0;
          end
          else
            Group[j].hClientCb := Results^[j].hServer;
      for j := 0 to Group.Count - 1 do
        CoTaskMemFree(Results^[j].pBlob);
      CoTaskMemFree(Results);
      CoTaskMemFree(Errors);
    finally
      SetLength(ItemDef, 0);
    end;
  finally
    SetLength(ItemIDs, 0);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////

{ Разорвать соединение с OPC сервером }
procedure TOPCClient.Disconnect;
begin
  if not FActive then
    Exit;
  DeleteGroups;
  // Уничтожаем OPC сервер
  m_pIOPCServer := nil;
  FActive := False;
end;

{ Удалить группы }
procedure TOPCClient.DeleteGroups;
var
  Group:     TGroup;
  j:         Integer;
  HRes:      HRESULT;
begin
  for j := 0 to FTagList.Count - 1 do
  begin
    Group := FTagList[j];
    DeleteTags(Group);
    // Удаляем группу
    Group.m_pIOPCItemMgt := nil;
    HRes := m_pIOPCServer.RemoveGroup(Group.hSGroupCb, True);
    if Failed(HRes) then
      log.WarningMsgFmt(msgErrDelGroup, [Group.GroupName]);
  end;
end;

{ Удаление тегов }
procedure TOPCClient.DeleteTags(const Group: TGroup);
var
  i:         Integer;
  HRes:      HRESULT;
  Errors:    PResultList;
  m_hSItems: array of OPCHANDLE;
begin
  if Group.Count < 1 then
    Exit;
  // Удалить тэги
  SetLength(m_hSItems, Group.Count);
  try
    for i := 0 to Group.Count - 1 do
      m_hSItems[i] := Group[i].hClientCb;
    HRes := Group.m_pIOPCItemMgt.RemoveItems(Group.Count, @m_hSItems[0], Errors);
    if Failed(HRes) then
      log.WarningMsgFmt(msgErrDelTag, [Group.GroupName]);
    for i := 0 to Group.Count - 1 do
      if Failed(Errors^[i]) then
        log.WarningMsgFmt(msgErrDelItem, [Group[i].TagName]);
    CoTaskMemFree(Errors);
  finally
    SetLength(m_hSItems, 0);
  end;
end;

{ Поиск группы }
function TOPCClient.FindSGroup(const aName: String): Integer;
begin
  Result := -1;
  if not Assigned(FTagList) then
    raise EOPCError.Create(msgNoListTag);
  Result := FTagList.GetNGroupName(aName);
  if Result = -1 then
    log.WarningMsgFmt(msgNoGrounName,[aName]);
end;

{ Поиск тега по индексу группы и имени тега }
function TOPCClient.FindSTag(const nG: Integer; const aName: String): TTagPosition;
begin
  with Result do
  begin
    NGroup := nG; NTag := -1;
    if nG = -1 then
      Exit;
    NTag := FTagList[nG].GetNTagName(aName);
    if NTag = -1 then
      log.WarningMsgFmt(msgNoTagName,[aName]);
  end;
end;

{ Поиск тега по наименованию группы и наименованию тега }
function TOPCClient.FindSGroupSTag(const GName, TName: String): TTagPosition;
begin
  Result := FindSTag(FindSGroup(GName), TName);
end;

{ }
function TOPCClient.GetTagQuality(const aTagPosition: TTagPosition): Word;
begin
  Result := 0; { WordRec(Result).Lo = OPC_QUALITY_BAD}
  if not FActive then
    Exit;
  try
    with aTagPosition do
      Result := FTagList[nGroup][nTag].Quality;
  except
    on EListError do Exit; //raise EOPCError.Create(msgErrNoTag);
  end;
end;

{ Установить значение в OPC сервер }
procedure TOPCClient.SetOPC(const aTagPosition: TTagPosition; const Val: OleVariant);
var
  HRes:   HRESULT;
  Errors: PResultList;
begin
  with aTagPosition do
    HRes := FTagList[nGroup].m_pIOPCSyncIO.Write(1, @FTagList[nGroup][nTag].hClientCb,
                                                 @Val, Errors);
  CoTaskMemFree(Errors);
  if Failed(HRes) then
    log.WarningMsg(msgErrRecOPCTag);
end;

{ Получить значение из OPC сервера }
function TOPCClient.GetOPC(const aTagPosition: TTagPosition): OleVariant;
var
  HRes:   HRESULT;
  Errors: PResultList;
  val: POPCITEMSTATEARRAY;
begin
  with aTagPosition do
    {Ожидаемое поведение заключается в том, что чтение CACHE должно выполняться
    очень быстро (в миллисекундах). Чтение УСТРОЙСТВА может занять очень много
    времени (много секунд или более). В зависимости от деталей реализации
    (например, какая модель потоковой передачи используется) считывание DEVICE
    также может препятствовать выполнению любых других операций на сервере
    любыми другими клиентами.
    По этой причине клиенты, как ожидается, будут использовать чтения CACHE
    в большинстве случаев. Чтения DEVICE предназначены для «особых»
    обстоятельств, таких как диагностика.            V}
    HRes := FTagList[nGroup].m_pIOPCSyncIO.Read(OPC_DS_CACHE, 1,
                                                @FTagList[nGroup][nTag].hClientCb,
                                                val, Errors);
  CoTaskMemFree(Errors);
  if Failed(HRes) then
    log.WarningMsg(msgErrRecOPCTag);
  Result := val^[0].vDataValue;
  CoTaskMemFree(val);
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Boolean}
{VT_BOOL (True=-1, False=0) (VARIANT_BOOL)}
function TOPCClient.GetTagBoolean(const aTagPosition: TTagPosition): Boolean;
begin
  Result := False;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
       begin
         Value.SetValue(GetOPC(aTagPosition));
         if (Value.Typ <> VT_BOOL) and ((Access <> acRead) or (Access <> acReadWrite)) then
           Exit;
         Result := Value.GetVT_BOOL;
       end;
  except
    on EListError do Exit;
  end;
end;

{Запись значений Boolean}
{VT_BOOL (True=-1, False=0) (VARIANT_BOOL)}
procedure TOPCClient.SetTagBoolean(const aTagPosition: TTagPosition;
                                   const Val: Boolean);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_BOOL) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запись значений Boolean}
{VT_BOOL (True=-1, False=0) (VARIANT_BOOL)}
procedure TOPCClient.SetQuiqTagBoolean(const aTagPosition: TTagPosition;
                                       const Val: Boolean);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_BOOL) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_BOOL(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений SmallInt}
{VT_I2 (2 byte signed int)(SmallInt)}
function TOPCClient.GetTagSmallInt(const aTagPosition: TTagPosition): SmallInt;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_I2) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_I2;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений SmallInt}
{VT_I2 (2 byte signed int)(SmallInt)}
procedure TOPCClient.SetTagSmallInt(const aTagPosition: TTagPosition; const Val: SmallInt);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I2) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений SmallInt}
{VT_I2 (2 byte signed int)(SmallInt)}
procedure TOPCClient.SetQuiqTagSmallInt(const aTagPosition: TTagPosition; const Val: SmallInt);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I2) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_I2(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений LongInt}
{VT_I4 (4 byte signed int)(Longint)}
{VT_INT (signed machine int)(Longint)}
function TOPCClient.GetTagLongint(const aTagPosition: TTagPosition): LongInt;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if ((Value.Typ <> VT_I4) or (Value.Typ <> VT_INT)) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_I4;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений LongInt}
{VT_I4 (4 byte signed int)(Longint)}
{VT_INT (signed machine int)(Longint)}
procedure TOPCClient.SetTagLongint(const aTagPosition: TTagPosition; const Val: LongInt);
begin
  if not FActive then
    Exit;

  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      if (Value.Typ in [VT_I4,VT_INT]) and ((Access <> acWrite) or (Access <> acReadWrite)) then
        Exit
      else
        SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений LongInt}
{VT_I4 (4 byte signed int)(Longint)}
{VT_INT (signed machine int)(Longint)}
procedure TOPCClient.SetQuiqTagLongint(const aTagPosition: TTagPosition; const Val: LongInt);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ in [VT_I4,VT_INT]) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_I4(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений ShortInt}
{VT_I1 (signed char)(ShortInt)}
function TOPCClient.GetTagShortInt(const aTagPosition: TTagPosition): ShortInt;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_I1) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_I1;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений ShortInt}
{VT_I1 (signed char)(ShortInt)}
procedure TOPCClient.SetTagShortInt(const aTagPosition: TTagPosition; const Val: ShortInt);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I1) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений ShortInt}
{VT_I1 (signed char)(ShortInt)}
procedure TOPCClient.SetQuiqTagShortInt(const aTagPosition: TTagPosition; const Val: ShortInt);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I4) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_I4(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Int64}
{VT_I8 (signed 64-bit int)(Int64)}
function TOPCClient.GetTagInt64(const aTagPosition: TTagPosition): Int64;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_I8) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_I8;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Int64}
{VT_I8 (signed 64-bit int)(Int64)}
procedure TOPCClient.SetTagInt64(const aTagPosition: TTagPosition; const Val: Int64);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Int64}
{VT_I8 (signed 64-bit int)(Int64)}
procedure TOPCClient.SetQuiqTagInt64(const aTagPosition: TTagPosition; const Val: Int64);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_I8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_I8(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Byte}
{VT_UI1 (unsigned char)(Byte)}
function TOPCClient.GetTagByte(const aTagPosition: TTagPosition): Byte;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_UI1) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_UI1;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Byte}
{VT_UI1 (unsigned char)(Byte)}
procedure TOPCClient.SetTagByte(const aTagPosition: TTagPosition; const Val: Byte);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI1) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Byte}
{VT_UI1 (unsigned char)(Byte)}
procedure TOPCClient.SetQuiqTagByte(const aTagPosition: TTagPosition; const Val: Byte);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI1) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_UI1(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Word}
{VT_UI2 (unsigned short)(Word)}
function TOPCClient.GetTagWord(const aTagPosition: TTagPosition): Word;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_UI2) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_UI2;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Word}
{VT_UI2 (unsigned short)(Word)}
procedure TOPCClient.SetTagWord(const aTagPosition: TTagPosition; const Val: Word);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI2) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Word}
{VT_UI2 (unsigned short)(Word)}
procedure TOPCClient.SetQuiqTagWord(const aTagPosition: TTagPosition; const Val: Word);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI2) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_UI2(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений LongWord}
{VT_UI4 (unsigned long)(LongWord)}
{VT_UINT (unsigned machine int)(LongWord)}
function TOPCClient.GetTagLongWord(const aTagPosition: TTagPosition): LongWord;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if ((Value.Typ <> VT_UI4) or (Value.Typ <> VT_UINT)) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_UI4;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений LongWord}
{VT_UI4 (unsigned long)(LongWord)}
{VT_UINT (unsigned machine int)(LongWord)}
procedure TOPCClient.SetTagLongWord(const aTagPosition: TTagPosition; const Val: LongWord);
begin
  if not FActive then
    Exit;

  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if ((Value.Typ <> VT_UI4) or (Value.Typ <> VT_UINT)) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений LongWord}
{VT_UI4 (unsigned long)(LongWord)}
{VT_UINT (unsigned machine int)(LongWord)}
procedure TOPCClient.SetQuiqTagLongWord(const aTagPosition: TTagPosition; const Val: LongWord);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;

  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if ((Value.Typ <> VT_UI4) or (Value.Typ <> VT_UINT)) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_UI4(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений QWord}
{VT_UI8 (unsigned 64-bit int)(QWord)}
function TOPCClient.GetTagQWord(const aTagPosition: TTagPosition): QWord;
begin
  Result := 0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_UI8) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_UI8;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запись значений QWord}
{VT_UI8 (unsigned 64-bit int)(QWord)}
procedure TOPCClient.SetTagQWord(const aTagPosition: TTagPosition; const Val: QWord);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений QWord}
{VT_UI8 (unsigned 64-bit int)(QWord)}
procedure TOPCClient.SetQuiqTagQWord(const aTagPosition: TTagPosition; const Val: QWord);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_UI8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_UI8(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Currency}
{VT_CY (Currency)}
function TOPCClient.GetTagCurrency(const aTagPosition: TTagPosition): Currency;
begin
  Result := 0.0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_CY) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_CY;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Currency}
{VT_CY (Currency)}
procedure TOPCClient.SetTagCurrency(const aTagPosition: TTagPosition; const Val: Currency);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_CY) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Currency}
{VT_CY (Currency)}
procedure TOPCClient.SetQuiqTagCurrency(const aTagPosition: TTagPosition; const Val: Currency);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_CY) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_CY(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Single}
{VT_R4 (4 byte real)(Single)}
function TOPCClient.GetTagSingle(const aTagPosition: TTagPosition): Single;
begin
  Result := 0.0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_R4) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_R4;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Single}
{VT_R4 (4 byte real)(Single)}
procedure TOPCClient.SetTagSingle(const aTagPosition: TTagPosition; const Val: Single);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_R4) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Single}
{VT_R4 (4 byte real)(Single)}
procedure TOPCClient.SetQuiqTagSingle(const aTagPosition: TTagPosition; const Val: Single);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_R4) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_R4(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений Double}
{VT_R8 (8 byte real)(Double)}
function TOPCClient.GetTagDouble(const aTagPosition: TTagPosition): Double;
begin
  Result := 0.0;
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_R8) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_R8;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений Double}
{VT_R8 (8 byte real)(Double)}
procedure TOPCClient.SetTagDouble(const aTagPosition: TTagPosition; const Val: Double);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_R8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений Double}
{VT_R8 (8 byte real)(Double)}
procedure TOPCClient.SetQuiqTagDouble(const aTagPosition: TTagPosition; const Val: Double);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_R8) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_R8(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////
{Чтение значений DateTime}
{VT_DATE (date)(TOleDate)}
function TOPCClient.GetTagDateTime(const aTagPosition: TTagPosition): TDateTime;
begin
  Result := EncodeDate(0, 0, 0) + EncodeTime(0, 0, 0, 0);
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if (Value.Typ <> VT_DATE) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetVT_DATE;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений DateTime}
{VT_DATE (date)(TOleDate)}
procedure TOPCClient.SetTagDateTime(const aTagPosition: TTagPosition; const Val: TDateTime);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      if (Value.Typ <> VT_DATE) and ((Access <> acWrite) or (Access <> acReadWrite)) then
        Exit
      else
        SetOPC(aTagPosition, Val);
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений DateTime}
{VT_DATE (date)(TOleDate)}
procedure TOPCClient.SetQuiqTagDateTime(const aTagPosition: TTagPosition; const Val: TDateTime);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if (Value.Typ <> VT_DATE) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          SetOPC(aTagPosition, Val);
          Value.SetVT_DATE(Val);
        end;
  except
    on EListError do Exit;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
{Чтение значений String}
{VT_BSTR (OLE Automation string)(POleStr (WideString))
 (Строка переменной длины, для храпения каждого символа используется 2 байта)}
{VT_LPSTR (null terminated string)(PChar) (Указатель на строку, 4 байта)}
{VT_LPWSTR (wide null terminated string)(PWideChar)
 (Указатель па строку, в который для храпения каждого символа
  используют 2 байта. Размер — 4 байта)}
function TOPCClient.GetTagString(const aTagPosition: TTagPosition): String;
begin
  Result := '';
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
      begin
        Value.SetValue(GetOPC(aTagPosition));
        if not (Value.Typ in [VT_BSTR, VT_LPSTR, VT_LPWSTR]) and ((Access <> acRead) or (Access <> acReadWrite)) then
          Exit;
        Result := Value.GetString;
      end;
  except
    on EListError do Exit;
  end;
end;

{Запить значений String}
{VT_BSTR (OLE Automation string)(POleStr (WideString))
 (Строка переменной длины, для храпения каждого символа используется 2 байта)}
{VT_LPSTR (null terminated string)(PChar) (Указатель на строку, 4 байта)}
{VT_LPWSTR (wide null terminated string)(PWideChar)
 (Указатель па строку, в который для храпения каждого символа
  используют 2 байта. Размер — 4 байта)}
procedure TOPCClient.SetTagString(const aTagPosition: TTagPosition; const Val: String);
begin
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if not (Value.Typ in [VT_BSTR, VT_LPSTR, VT_LPWSTR]) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
          if Value.Typ = VT_LPSTR then
            SetOPC(aTagPosition, UTF8Decode(Val)) //?????
          else
            SetOPC(aTagPosition, UTF8Decode(Val));
  except
    on EListError do Exit;
  end;
end;

{Быстрая запить значений String}
{VT_BSTR (OLE Automation string)(POleStr (WideString))
 (Строка переменной длины, для храпения каждого символа используется 2 байта)}
{VT_LPSTR (null terminated string)(PChar) (Указатель на строку, 4 байта)}
{VT_LPWSTR (wide null terminated string)(PWideChar)
 (Указатель па строку, в который для храпения каждого символа
  используют 2 байта. Размер — 4 байта)}
procedure TOPCClient.SetQuiqTagString(const aTagPosition: TTagPosition; const Val: String);
begin
  // Записяваем в FTagList и потом в OPCSerwer
  if not FActive then
    Exit;
  try
    with aTagPosition do
      with FTagList[nGroup][nTag] do
        if not (Value.Typ in [VT_BSTR, VT_LPSTR, VT_LPWSTR]) and ((Access <> acWrite) or (Access <> acReadWrite)) then
          Exit
        else
        begin
          if Value.Typ = VT_LPSTR then
            SetOPC(aTagPosition, UTF8Decode(Val)) //?????
          else
            SetOPC(aTagPosition, UTF8Decode(Val));
          Value.SetString(Val);
        end;
  except
    on EListError do Exit;
  end;
end;


procedure TOPCClient.SetServerName(AValue: String);
begin
  if FServerName = AValue then
    Exit;
  FServerName := AValue;
end;

procedure TOPCClient.ReadListTag(Reader: TReader);
begin
  FTagList.Clear;
  FTagList.ReadTagList(Reader);
end;

procedure TOPCClient.WriteListTag(Writer: TWriter);
begin
  FTagList.WriteTagList(Writer);
end;

procedure TOPCClient.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TagList', @ReadListTag, @WriteListTag, True);
end;

end.
