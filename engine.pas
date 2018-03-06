unit engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    XmlRpcServer, XmlRpcTypes,
    dictionary, settings, obj_proto;

{ Режимы запуска движка }
const
  RUN_MODE_SINGLE: AnsiString = 'single';
  RUN_MODE_LOOP: AnsiString = 'loop';
  RUN_MODE_DIAGNOSTIC: AnsiString = 'diagnostic';

  DEFAULT_XML_RPC_PORT: Integer = 8080;

type
    {
    TICReaderProto - абстрактный тип движка
    ЧИТАТЕЛЯ ДАННЫХ из различных источников
    }

    TICReaderProto = class(TObject)
    private
      { Менеджер настроек }
      FSettingsManager: TICSettingsManager;
      { Словарь зарегистрированных объектов }
      FObjects: TStrDictionary;

      { Сервер удаленного вызова процедур }
      FRpcServer: TRpcServer;
    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      { Проинициализировать конфигурационные переменные в соответствии с настройками }
      function InitSettings(): Boolean;
      { Регистрация нового объекта в словаре внутренних объектов. Регистрация производиться по имени объекта. }
      function RegObject(Obj: TICObjectProto): Boolean;
      { Поиск объекта в зарегистрированных по имени }
      function FindObject(sObjName: AnsiString): TICObjectProto;
      { Метод создания объекта контроллера данных с инициализацией его свойств }
      function CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;

      { Создание объектов по именам }
      function CreateDataControllers(ObjectNames: TStringList=nil): TList;

    end;

    {
    TICReader - Движок ЧИТАТЕЛЯ ДАННЫХ из различных источников
    }

    TICReader = class(TICReaderProto)
    private
      { Основная процедура запуска }
      //function DoRun(sMode: AnsiString): Boolean;

    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      { Запуск всех объектов для выполнения 1 тика }
      //function RunTick(iTick: Integer = 1): Boolean;
      { Основная процедура запуска }
      //function Run(sMode: AnsiString): Boolean;
      { Произвести диагностику объектов }
      //function DoDiagnostic(lObjects: TStringList): Boolean;

      { Прочитать значение из источника данных }
      function ReadValueAsString(sSrcTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString): AnsiString;
      { Прочитать список значений из источника данных }
      function ReadValuesAsStrings(sSrcTypeName: AnsiString; const aArgs : Array Of Const; aAddresses : Array Of String): TStringList;

      { Инициализировать методы удаленного вызова }
      // procedure RegRpcMethods;
      procedure StartServer;
      procedure StopServer;

      { --- Используемые процедуры удаленного вызова --- }
      { Тестовая функция для проверки удаленного вызова процедур }
      procedure EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                  List: TList; Return: TRpcReturn);

      { Функция чтения данных из источника удаленного вызова процедур }
      procedure ReadValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                           List: TList; Return: TRpcReturn);

      { Функция чтения данных из источника удаленного вызова процедур }
      procedure ReadValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                             List: TList; Return: TRpcReturn);
    end;

var
  { Порт по умолчанию для обработки XML RPC }
  XML_RPC_PORT: Integer = 8080;

  {
  Объявление глобального объекта движка

  ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
  Переменные определенные в секции implementation являются статическими для
  модуля.
  }
  READER_ENGINE: TICReader;



implementation

uses
  log, config, reg_data_ctrl, keyboardfunc, strfunc, memfunc;

constructor TICReaderProto.Create(TheOwner: TComponent);
begin
  inherited Create;

  // Менеджер настроек
  FSettingsManager := TICSettingsManager.Create;

  // Словарь зарегистрированных объектов
  FObjects := TStrDictionary.Create;

end;

destructor TICReaderProto.Destroy;
begin
  //Free;
  FRpcServer.Free;
  inherited Destroy;
end;

procedure TICReaderProto.Free;
begin
  FObjects.Free;
  FSettingsManager.Free;
  inherited Free;
end;

{
Проинициализировать конфигурационные переменные в соответствии с настройками.
@return (True/False)
}
function TICReaderProto.InitSettings():Boolean;
var
  ini_filename: AnsiString;
begin
    if not ENVIRONMENT.HasKey('SETTINGS_FILENAME') then
    begin
       ini_filename := FSettingsManager.GenIniFileName();
       ENVIRONMENT.AddStrValue('SETTINGS_FILENAME', ini_filename);
    end
    else
        ini_filename := ENVIRONMENT.GetStrValue('SETTINGS_FILENAME');

    log.DebugMsgFmt('INI Файл <%s>', [ini_filename]);
    if (ini_filename <> '') and (not FileExists(ini_filename)) then
    begin
        log.WarningMsgFmt('Файл настроек <%s> не найден. Используется файл настроек по умолчанию', [ini_filename]);
        ini_filename := '';
    end;
    result := FSettingsManager.LoadSettings(ini_filename);

    FSettingsManager.PrintSettings;
end;

{
Регистрация нового объекта в словаре внутренних объектов.
Регистрация производиться по имени объекта.
@param (Obj Регистрируемый объект)
@return (True -  регистрация прошла успешно / False - ошибка)
}
function TICReaderProto.RegObject(Obj: TICObjectProto): Boolean;
var
  name: AnsiString;
begin
    if not obj.IsUnknown then
    begin
        // Регистрация по имени
        name := obj.GetName();
        FObjects.AddObject(name, obj);
        result := True;
        exit;
    end
    else
        log.WarningMsgFmt('Не возможно зарегистрировать объект класса <%s>', [obj.ClassName]);
    result := False;
end;

{
Поиск объекта в зарегистрированных по имени.
@param obj_name: Имя объекта.
@return: Объект или None если объект с таким именем не найден.
}
function TICReaderProto.FindObject(sObjName: AnsiString): TICObjectProto;
begin
    if FObjects.HasKey(sObjName) then
        result := FObjects.GetByName(sObjName) As TICObjectProto;
    log.WarningMsgFmt('Объект <%s> не найден среди зарегистрированных %s', [sObjName, FObjects.GetKeysStr()]);
    result := nil;
end;

{
Метод создания объекта контроллера данных с инициализацией его свойств.
@param (Properties  Словарь свойств контроллера данных)
@return (Объект контроллера данных или nil в случае ошибки)
}
function TICReaderProto.CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;
var
   type_name, name: AnsiString;
   ctrl_obj: TICObjectProto;
begin
    // Сначала в любом случае определяем тип источника данных
    if Properties.HasKey('type') then
    begin
        type_name := Properties.GetStrValue('type');
        ctrl_obj := CreateRegDataCtrl(self, type_name, Properties);
        if ctrl_obj <> nil then
        begin
             //ctrl_obj.SetParent(self);
             //ctrl_obj.SetProperties(Properties);
             // Регистрируем новый объект в словаре внутренних объектов
             RegObject(ctrl_obj);
             result := ctrl_obj;
             exit;
        end;
    end
    else
    begin
        name := Properties.GetStrValue('name');
        log.ErrorMsgFmt('Ошибка создания объекта источника данных. Не определен тип <%s>', [name]);
    end;
    result := nil;
end;

{
Создание объектов по именам
}
function TICReaderProto.CreateDataControllers(ObjectNames: TStringList): TList;
var
   ctrl_objects: TList;
   obj: TICObjectProto;
   obj_names_str: AnsiString;
   i: Integer;
   obj_properties: TStrDictionary;
   is_obj_names_options: Boolean;
begin
    log.InfoMsg('Создание объектов...');
    ctrl_objects := TList.Create;
    is_obj_names_options := False;
    if ObjectNames = nil then
    begin
         obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'objects');
         ObjectNames := ParseStrList(obj_names_str);
         is_obj_names_options := True;
    end;

    for i := 0 to ObjectNames.Count - 1 do
    begin
        obj_properties := FSettingsManager.BuildSection(ObjectNames[i]);

        // Создаем объекты источников данных
        obj := CreateDataCtrl(obj_properties);
        if obj <> nil then
            ctrl_objects.Add(obj)
    end;

    // Освободить память если мы выделяли
    if is_obj_names_options then
       ObjectNames.Free;

    result := ctrl_objects;
end;

constructor TICReader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TICReader.Destroy;
begin
  inherited Destroy;
end;

procedure TICReader.Free;
begin
  inherited Free;
end;

{
Запуск всех объектов для выполнения 1 тика.
@param (iTick  Номер текущего тика)
@return (True/False)
}
//function TICReader.RunTick(iTick: Integer): Boolean;
//var
//   ctrl_objects: TList;
//   // obj_names: TStringList;
//   i: Integer;
//   // obj_properties: TStrDictionary;
//   obj: TICObjectProto;
//begin
//    try
//        ENVIRONMENT.SetDateTimeValue('TICK_DT_START', Now);
//        // ВНИМАНИЕ! Необходимо с начале каждого тика надо создавать объекты
//        // чтобы не контролировать актуальность их состояния
//        ctrl_objects := CreateDataControllers();
//
//        log.InfoMsg('Начало обработки...');
//        for i := 0 to ctrl_objects.Count - 1 do
//        begin
//             obj := TObject(ctrl_objects.Items[i]) As TICObjectProto;
//             obj.Read(obj.GetReadValues());
//        end;
//        log.InfoMsgFmt('...Конец обработки [%d]', [iTick]);
//
//        result := True;
//        exit;
//    except
//        log.FatalMsgFmt('Ошибка выполнения тика [%d]', [iTick]);
//    end;
//    result := False;
//end;

{
Основная процедура запуска.
@param (sMode Режим запуска)
@return (True/False)
}
//function TICReader.DoRun(sMode: AnsiString): Boolean;
//var
//   do_exit: Boolean;
//   tick, start_tick, end_tick, cur_time: TDateTime;
//   ch_key: Char;
//   obj_names_str: AnsiString;
//   obj_names: TStringList;
//begin
//    // Проинициализировать конфигурационные переменные в соответствии с настройками.
//    InitSettings();
//
//    if sMode = '' then
//        sMode := ENVIRONMENT.GetStrValue('RUN_MODE');
//
//    if sMode = RUN_MODE_SINGLE then
//    begin
//        // Одноразовый запуск
//        RunTick();
//        ENVIRONMENT.SetDateTimeValue('TICK_DT_STOP', Now);
//    end
//    else if sMode = RUN_MODE_LOOP then
//    begin
//        // Запуск регистратора в цикле
//        // Запуск цикла обработки
//        do_exit := False;
//        tick := ENVIRONMENT.GetDateTimeValue('TICK_PERIOD');
//        log.InfoMsgFmt('Период цикла обработки: <%d>...', [tick]);
//        while not do_exit do
//        begin
//            start_tick := Now;
//            end_tick := start_tick + tick;
//
//            RunTick(0);
//
//            if tick > 0 then
//            begin
//                log.WarningMsg('Для выхода нажмите <ESC>');
//                cur_time := Now;
//                while end_tick > cur_time do
//                begin
//                    ch_key := ReadKey();
//                    if SameKey(ch_key, ESC_KEY) then
//                        do_exit := True;
//                        log.InfoMsg('Выход из цикла обработки');
//                        break;
//                    cur_time := Now;
//                end;
//            end
//            else
//                log.WarningMsg('Для выхода нажмите <Ctrl + C>');
//
//            ENVIRONMENT.SetDateTimeValue('TICK_DT_STOP', Now);
//            log.InfoMsg('...Конец периода цикла обработки');
//        end
//    end
//    else if sMode = RUN_MODE_DIAGNOSTIC then
//    begin
//         obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'objects');
//         obj_names := ParseStrList(obj_names_str);
//         // DebugMsg(Format('Объекты <%s> : %s', [obj_names_str, obj_names.Text]));
//         // Запуск регистратора в режиме диагностики источников данных
//         DoDiagnostic(obj_names);
//         obj_names.Free;
//    end
//    else
//    begin
//        log.WarningMsgFmt('Режим запуска регистратра <%s> не поддерживается системой', [sMode]);
//        result := False;
//        exit;
//    end;
//    result := True;
//end;

{
Основная процедура запуска движка.
@param (sMode Режим запуска)
@return (True/False)
}
//function TICReader.Run(sMode: AnsiString): Boolean;
//begin
//    result := False;
//    try
//      RegRpcMethods;
//      result := DoRun(sMode);
//    except
//      log.FatalMsgFmt('Ошибка запуска <%s>', [ClassName]);
//    end;
//end;

{
Произвести диагностику объектов.
@param (lObjects Список имен объектов. В нашем случае это OBJECTS из окружения.)
@return (True/False)
}
//function TICReader.DoDiagnostic(lObjects: TStringList): Boolean;
//var
//   i: Integer;
//   obj_properties: TStrDictionary;
//   obj: TICObjectProto;
//begin
//    result := True;
//    log.InfoMsgFmt('Запуск диагностики объектов %s', [ConvertStrListToString(lObjects)]);
//    for i := 0 to lObjects.Count - 1 do
//    begin
//        obj_properties := FSettingsManager.BuildSection(lObjects[i]);
//        obj := CreateDataCtrl(obj_properties);
//        if obj <> nil then
//            result := result and obj.Diagnostic();
//        obj_properties.Free;
//    end;
//end;

{ Прочитать значение из источника данных }
function TICReader.ReadValueAsString(sSrcTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString): AnsiString;
var
  ctrl_obj: TICObjectProto;
  str_list: TStringList;

begin
  Result := '';
  ctrl_obj := nil;
  str_list := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sSrcTypeName, aArgs);
    str_list := ctrl_obj.ReadAddresses([sAddress]);
    Result := str_list.Strings[0];
  except
    log.FatalMsgFmt('Ошибка чтения значения по адресу <%s>', [sAddress]);
  end;

  if str_list <> nil then
    str_list.Free;
  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Прочитать список значений из источника данных }
function TICReader.ReadValuesAsStrings(sSrcTypeName: AnsiString; const aArgs: Array Of Const; aAddresses: Array Of String): TStringList;
var
  ctrl_obj: TICObjectProto;
  str_list: TStringList;

begin
  Result := nil;
  ctrl_obj := nil;
  str_list := nil;

  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sSrcTypeName, aArgs);
    str_list := ctrl_obj.ReadAddresses(aAddresses);
    Result := str_list;
  except
    log.FatalMsg('Ошибка чтения значений по адресам:');
  end;

  //if str_list <> nil then
  //  str_list.Free;
  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Инициализировать методы удаленного вызова }
//procedure TICReader.RegRpcMethods;
//var
//  EchoTestRpcMethodHandler: TRpcMethodHandler;
//  ReadValueAsStringRpcMethodHandler: TRpcMethodHandler;
//  ReadValuesAsStringsRpcMethodHandler: TRpcMethodHandler;
//
//begin
//  if not Assigned(FRpcServer) then
//  begin
//    FRpcServer := TRpcServer.Create;
//    FRpcServer.ListenPort := XML_RPC_PORT;
//    FRpcServer.EnableIntrospect := True;
//
//    EchoTestRpcMethodHandler := TRpcMethodHandler.Create;
//    ReadValueAsStringRpcMethodHandler := TRpcMethodHandler.Create;
//    ReadValuesAsStringsRpcMethodHandler := TRpcMethodHandler.Create;
//
//    try
//      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
//      EchoTestRpcMethodHandler.Name := 'tests.echoString';
//      // ВНИМАНИЕ! В Lazarus необходимо указывать @ для связки события с обработчиком
//      //                         V
//      EchoTestRpcMethodHandler.Method := @EchoTestRpcMethod;
//      EchoTestRpcMethodHandler.Signature := 'string (string myval)';
//      EchoTestRpcMethodHandler.Help := 'Just a simple test rpc example method';
//      FRpcServer.RegisterMethodHandler(EchoTestRpcMethodHandler);
//      //EchoTestRpcMethodHandler.Free;
//      EchoTestRpcMethodHandler := nil;
//      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//
//      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
//      ReadValueAsStringRpcMethodHandler.Name := 'sources.ReadValueAsString';
//      ReadValueAsStringRpcMethodHandler.Method := @ReadValueAsStringRpcMethod;
//      ReadValueAsStringRpcMethodHandler.Signature := 'string (string myval)';
//      ReadValueAsStringRpcMethodHandler.Help := 'Read value as string from data source';
//      FRpcServer.RegisterMethodHandler(ReadValueAsStringRpcMethodHandler);
//      //ReadValueAsStringRpcMethodHandler.Free;
//      ReadValueAsStringRpcMethodHandler := nil;
//      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//
//      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
//      ReadValuesAsStringsRpcMethodHandler.Name := 'sources.ReadValuesAsStrings';
//      ReadValuesAsStringsRpcMethodHandler.Method := @ReadValuesAsStringsRpcMethod;
//      ReadValuesAsStringsRpcMethodHandler.Signature := 'string (string myval)';
//      ReadValuesAsStringsRpcMethodHandler.Help := 'Read values as strings from data source';
//      FRpcServer.RegisterMethodHandler(ReadValuesAsStringsRpcMethodHandler);
//      //ReadValuesAsStringsRpcMethodHandler.Free;
//      ReadValuesAsStringsRpcMethodHandler := nil;
//      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//
//      FRpcServer.Active := True;
//    finally
//      EchoTestRpcMethodHandler.Free;
//      ReadValueAsStringRpcMethodHandler.Free;
//      ReadValuesAsStringsRpcMethodHandler.Free;
//
//      //EchoTestRpcMethodHandler := nil;
//      //ReadValueAsStringRpcMethodHandler := nil;
//      //ReadValuesAsStringsRpcMethodHandler := nil;
//    end;
//  end;
//end;

procedure TICReader.StartServer;
var
  MethodHandler: TRpcMethodHandler;
  //EchoTestRpcMethodHandler: TRpcMethodHandler;
  //ReadValueAsStringRpcMethodHandler: TRpcMethodHandler;
  //ReadValuesAsStringsRpcMethodHandler: TRpcMethodHandler;

begin
  if not Assigned(FRpcServer) then
  begin
    FRpcServer := TRpcServer.Create;
    FRpcServer.ListenPort := XML_RPC_PORT;
    FRpcServer.EnableIntrospect := True;

    //EchoTestRpcMethodHandler := TRpcMethodHandler.Create;
    //ReadValueAsStringRpcMethodHandler := TRpcMethodHandler.Create;
    //ReadValuesAsStringsRpcMethodHandler := TRpcMethodHandler.Create;

    try
      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'tests.echoString';
      // ВНИМАНИЕ! В Lazarus необходимо указывать @ для связки события с обработчиком
      //                         V
      MethodHandler.Method := @EchoTestRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Just a simple test rpc example method';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      //EchoTestRpcMethodHandler.Free;
      // EchoTestRpcMethodHandler := nil;
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'sources.ReadValueAsString';
      MethodHandler.Method := @ReadValueAsStringRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Read value as string from data source';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      //ReadValueAsStringRpcMethodHandler.Free;
      //ReadValueAsStringRpcMethodHandler := nil;
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'sources.ReadValuesAsStrings';
      MethodHandler.Method := @ReadValuesAsStringsRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Read values as strings from data source';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      //ReadValuesAsStringsRpcMethodHandler.Free;
      //ReadValuesAsStringsRpcMethodHandler := nil;
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      FRpcServer.Active := True;
    except
      log.FatalMsg('Ошибка запуска XML RPC сервера');
      //EchoTestRpcMethodHandler.Free;
      //ReadValueAsStringRpcMethodHandler.Free;
      //ReadValuesAsStringsRpcMethodHandler.Free;

      //EchoTestRpcMethodHandler := nil;
      //ReadValueAsStringRpcMethodHandler := nil;
      //ReadValuesAsStringsRpcMethodHandler := nil;
    end;
  end;
end;

procedure TICReader.StopServer;
begin
  FRpcServer.Active := False;
end;

{ Тестовая функция для проверки удаленного вызова процедур }
procedure TICReader.EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                      List: TList; Return: TRpcReturn);
var
  Msg: string;

begin
  {The parameter list is sent to your method as a TList of parameters
   this must be casted to a parameter to be accessed. If a error occurs
   during the execution of your method the server will fall back to a global
   handler and try to recover in which case the stack error will be sent to
   the client}

  {grab the sent string}
  Msg := TRpcParameter(List[0]).AsString;

  log.DebugMsgFmt('Test echo. You just sent: <%s>', [Msg]);

  {return a message showing what was sent}
  Return.AddItem('You just sent: ' + Msg);
end;

{ Функция чтения данных из источника удаленного вызова процедур }
procedure TICReader.ReadValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                               List: TList; Return: TRpcReturn);
var
  src_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: AnsiString;

begin
  src_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;

  memfunc.InitStatusMemory();
  opc_result := ReadValueAsString(src_type_name, [opc_server_name], address);
  memfunc.PrintLostMemory();

  // log.DebugMsgFmt('2. Адрес: <%s> Значение: "%s"', [address, opc_result]);
  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;

{ Функция чтения данных из источника удаленного вызова процедур }
procedure TICReader.ReadValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                                 List: TList; Return: TRpcReturn);
var
  src_type_name: AnsiString;
  opc_server_name: AnsiString;
  addresses: Array of String;
  opc_result: TStringList;
  i: Integer;

begin
  src_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;

  SetLength(addresses, List.Count - 2);
  for i := 0 to List.Count - 3 do
  begin
    addresses[i] := TRpcParameter(List[i + 2]).AsString;
    log.DebugMsgFmt('Чтение тега <tag%d>. Адрес <%s>', [i, addresses[i]]);
  end;

  memfunc.InitStatusMemory();
  opc_result := ReadValuesAsStrings(src_type_name, [opc_server_name], addresses);
  addresses := nil;

  // log.DebugMsgFmt('2. Адрес: <%s> Значение: "%s"', [address, opc_result]);
  {return a message showing what was sent}
  if opc_result <> nil then
    for i := 0 to opc_result.Count - 1 do
    begin
      Return.AddItem(opc_result.Strings[i]);
    end;
    opc_result.Free;

  memfunc.PrintLostMemory();
end;

begin
  //READER_ENGINE := TICReader.Create(nil);

end.

