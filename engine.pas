unit engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    dictionary, settings, obj_proto;

{ Режимы запуска движка }
const RUN_MODE_SINGLE: AnsiString = 'single';
const RUN_MODE_LOOP: AnsiString = 'loop';
const RUN_MODE_DIAGNOSTIC: AnsiString = 'diagnostic';

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
      function CreateDataControllers(ObjectNames: TStringList=Nil): TList;

    end;

    {
    TICReader - Движок ЧИТАТЕЛЯ ДАННЫХ из различных источников
    }

    TICReader = class(TICReaderProto)
    private
      { Основная процедура запуска }
      function DoRun(sMode: AnsiString): Boolean;

    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      { Запуск всех объектов для выполнения 1 тика }
      function RunTick(iTick: Integer = 1): Boolean;
      { Основная процедура запуска }
      function Run(sMode: AnsiString): Boolean;
      { Произвести диагностику объектов }
      function DoDiagnostic(lObjects: TStringList): Boolean;
    end;

var
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

    DebugMsg(Format('INI Файл <%s>', [ini_filename]));
    if (ini_filename <> '') and (not FileExists(ini_filename)) then
    begin
        WarningMsg(Format('Файл настроек <%s> не найден. Используется файл настроек по умолчанию', [ini_filename]));
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
        WarningMsg(Format('Не возможно зарегистрировать объект класса <%s>', [obj.ClassName]));
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
    WarningMsg(Format('Объект <%s> не найден среди зарегистрированных %s', [sObjName, FObjects.GetKeysStr()]));
    result := Nil;
end;

{
Метод создания объекта контроллера данных с инициализацией его свойств.
@param (Properties  Словарь свойств контроллера данных)
@return (Объект контроллера данных или Nil в случае ошибки)
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
        if ctrl_obj <> Nil then
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
        ErrorMsg(Format('Ошибка создания объекта источника данных. Не определен тип <%s>', [name]));
    end;
    result := Nil;
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
    InfoMsg('Создание объектов...');
    ctrl_objects := TList.Create;
    is_obj_names_options := False;
    if ObjectNames = Nil then
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
        if obj <> Nil then
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
function TICReader.RunTick(iTick: Integer): Boolean;
var
   ctrl_objects: TList;
   // obj_names: TStringList;
   i: Integer;
   // obj_properties: TStrDictionary;
   obj: TICObjectProto;
begin
    try
        ENVIRONMENT.SetDateTimeValue('TICK_DT_START', Now);
        // ВНИМАНИЕ! Необходимо с начале каждого тика надо создавать объекты
        // чтобы не контролировать актуальность их состояния
        ctrl_objects := CreateDataControllers();

        InfoMsg('Начало обработки...');
        for i := 0 to ctrl_objects.Count - 1 do
        begin
             obj := TObject(ctrl_objects.Items[i]) As TICObjectProto;
             obj.Read(obj.GetReadValues());
        end;
        InfoMsg(Format('...Конец обработки [%d]', [iTick]));

        result := True;
        exit;
    except
        FatalMsg(Format('Ошибка выполнения тика [%d]', [iTick]));
    end;
    result := False;
end;

{
Основная процедура запуска.
@param (sMode Режим запуска)
@return (True/False)
}
function TICReader.DoRun(sMode: AnsiString): Boolean;
var
   do_exit: Boolean;
   tick, start_tick, end_tick, cur_time: TDateTime;
   ch_key: Char;
   obj_names_str: AnsiString;
   obj_names: TStringList;
begin
    // Проинициализировать конфигурационные переменные в соответствии с настройками.
    InitSettings();

    if sMode = '' then
        sMode := ENVIRONMENT.GetStrValue('RUN_MODE');

    if sMode = RUN_MODE_SINGLE then
    begin
        // Одноразовый запуск
        RunTick();
        ENVIRONMENT.SetDateTimeValue('TICK_DT_STOP', Now);
    end
    else if sMode = RUN_MODE_LOOP then
    begin
        // Запуск регистратора в цикле
        // Запуск цикла обработки
        do_exit := False;
        tick := ENVIRONMENT.GetDateTimeValue('TICK_PERIOD');
        InfoMsg(Format('Период цикла обработки: <%d>...', [tick]));
        while not do_exit do
        begin
            start_tick := Now;
            end_tick := start_tick + tick;

            RunTick(0);

            if tick > 0 then
            begin
                WarningMsg('Для выхода нажмите <ESC>');
                cur_time := Now;
                while end_tick > cur_time do
                begin
                    ch_key := ReadKey();
                    if SameKey(ch_key, ESC_KEY) then
                        do_exit := True;
                        InfoMsg('Выход из цикла обработки');
                        break;
                    cur_time := Now;
                end;
            end
            else
                WarningMsg('Для выхода нажмите <Ctrl + C>');

            ENVIRONMENT.SetDateTimeValue('TICK_DT_STOP', Now);
            InfoMsg('...Конец периода цикла обработки');
        end
    end
    else if sMode = RUN_MODE_DIAGNOSTIC then
    begin
         obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'objects');
         obj_names := ParseStrList(obj_names_str);
         // DebugMsg(Format('Объекты <%s> : %s', [obj_names_str, obj_names.Text]));
         // Запуск регистратора в режиме диагностики источников данных
         DoDiagnostic(obj_names);
         obj_names.Free;
    end
    else
    begin
        WarningMsg(Format('Режим запуска регистратра <%s> не поддерживается системой', [sMode]));
        result := False;
        exit;
    end;
    result := True;
end;

{
Основная процедура запуска движка.
@param (sMode Режим запуска)
@return (True/False)
}
function TICReader.Run(sMode: AnsiString): Boolean;
begin
    result := False;
    try
        result := DoRun(sMode);
    except
        FatalMsg(Format('Ошибка запуска <%s>', [ClassName]));
    end;
end;

{
Произвести диагностику объектов.
@param (lObjects Список имен объектов. В нашем случае это OBJECTS из окружения.)
@return (True/False)
}
function TICReader.DoDiagnostic(lObjects: TStringList): Boolean;
var
   i: Integer;
   obj_properties: TStrDictionary;
   obj: TICObjectProto;
begin
    result := True;
    InfoMsg(Format('Запуск диагностики объектов %s', [ConvertStrListToString(lObjects)]));
    for i := 0 to lObjects.Count - 1 do
    begin
        obj_properties := FSettingsManager.BuildSection(lObjects[i]);
        obj := CreateDataCtrl(obj_properties);
        if obj <> Nil then
            result := result and obj.Diagnostic();
        obj_properties.Free;
    end;
end;

begin
  READER_ENGINE := TICReader.Create(Nil);

end.

