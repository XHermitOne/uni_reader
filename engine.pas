unit engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    dictionary, settings, obj_proto, config, log, reg_data_ctrl, keyboardfunc;

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
      SettingsManager: TICSettingsManager;
      { Словарь зарегистрированных объектов }
      Objects: TStrDictionary;

    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;

      { Проинициализировать конфигурационные переменные в соответствии с настройками }
      function InitSettings(): Boolean;
      { Регистрация нового объекта в словаре внутренних объектов. Регистрация производиться по имени объекта. }
      function RegObject(Obj: TICObjectProto): Boolean;
      { Поиск объекта в зарегистрированных по имени }
      function FindObject(sObjName: AnsiString): TICObjectProto;
      { Метод создания объекта контроллера данных с инициализацией его свойств }
      function CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;

    end;

    {
    TICReader - Движок ЧИТАТЕЛЯ ДАННЫХ из различных источников
    }

    TICReader = class(TICReaderProto)
    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;

      { Запуск всех объектов для выполнения 1 тика }
      function RunTick(iTick: Integer = 1): Boolean;
      { Основная процедура запуска регистрации }
      function Run(sMode: AnsiString): Boolean;
      { Произвести диагностику объектов }
      function DoDiagnostic(lObjects: TStringList): Boolean;
    end;

implementation

constructor TICReaderProto.Create(TheOwner: TComponent);
begin
  inherited Create;

  // Менеджер настроек
  SettingsManager := TICSettingsManager.Create;

  // Словарь зарегистрированных объектов
  Objects := TStrDictionary.Create;

end;

destructor TICReaderProto.Destroy;
begin
  Objects.Free;
  SettingsManager.Free;
  inherited Destroy;
end;

{
Проинициализировать конфигурационные переменные в соответствии с настройками.
@return (True/False)
}
function TICReaderProto.InitSettings():Boolean;
var
  ini_filename: AnsiString;
begin
    ini_filename := ENVIRONMENT.GetStrValue('SETTINGS_FILENAME');
    if (ini_filename <> '') and (not FileExists(ini_filename)) then
    begin
        warning(Format('Файл настроек <%s> не найден. Используется файл настроек по умолчанию', [ini_filename]));
        ini_filename := '';
    end;
    result := SettingsManager.LoadSettings(ini_filename);
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
        Objects.AddObject(name, obj);
        result := True;
        exit;
    end
    else
        warning(Format('Не возможно зарегистрировать объект класса <%s>', [obj.ClassName]));
    result := False;
end;

{
Поиск объекта в зарегистрированных по имени.
@param obj_name: Имя объекта.
@return: Объект или None если объект с таким именем не найден.
}
function TICReaderProto.FindObject(sObjName: AnsiString): TICObjectProto;
begin
    if Objects.HasKey(sObjName) then
        result := Objects.GetByName(sObjName) As TICObjectProto;
    warning(Format('Объект <%s> не найден среди зарегистрированных %s', [sObjName, Objects.GetKeysStr()]));
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
        ctrl_obj := CreateRegDataCtrl(type_name);
        if ctrl_obj <> Nil then
        begin
             ctrl_obj.SetParent(self);
             ctrl_obj.SetProperties(Properties);
             // Регистрируем новый объект в словаре внутренних объектов
             RegObject(ctrl_obj);
             result := ctrl_obj;
             exit;
        end;
    end
    else
    begin
        name := Properties.GetStrValue('name');
        error(Format('Ошибка создания объекта источника данных. Не определен тип <%s>', [name]));
    end;
    result := Nil;
end;

constructor TICReader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TICReader.Destroy;
begin
  inherited Destroy;
end;

{
Запуск всех объектов для выполнения 1 тика.
@param (iTick  Номер текущего тика)
@return (True/False)
}
function TICReader.RunTick(iTick: Integer): Boolean;
var
   ctrl_objects: TList;
   obj_names: TStringList;
   i: Integer;
   obj_properties: TStrDictionary;
   obj: TICObjectProto;
begin
    try
        ENVIRONMENT.SetDateTimeValue('TICK_DT_START', Now);
        // ВНИМАНИЕ! Необходимо с начале каждого тика надо создавать объекты
        // чтобы не контролировать актуальность их состояния
        info('Создание объектов...');
        ctrl_objects := TList.Create;
        obj_names := ENVIRONMENT.GetStrList('OBJECTS');
        for i := 0 to obj_names.Count - 1 do
        begin
            obj_properties := SettingsManager.BuildSection(obj_names[i]);

            // Создаем объекты источников данных
            obj := CreateDataCtrl(obj_properties);
            if obj <> Nil then
                ctrl_objects.Add(obj)
        end;

        info('Начало обработки...');
        for i := 0 to obj_names.Count - 1 do
        begin
             obj := TObject(ctrl_objects.Items[i]) As TICObjectProto;
             obj.Read(obj.GetReadValues());
        end;
        info(Format('...Конец обработки [%d]', [iTick]));

        result := True;
        exit;
    except
        fatal(Format('Ошибка выполнения тика [%d]', [iTick]));
    end;
    result := False;
end;

{
Основная процедура запуска регистрации.
@param (sMode Режим запуска регистратора)
@return (True/False)
}
function TICReader.Run(sMode: AnsiString): Boolean;
var
   do_exit: Boolean;
   tick, start_tick, end_tick, cur_time: TDateTime;
   ch_key: Char;
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
        info(Format('Период цикла обработки: <%d>...', [tick]));
        while not do_exit do
        begin
            start_tick := Now;
            end_tick := start_tick + tick;

            RunTick(0);

            if tick > 0 then
            begin
                warning('Для выхода нажмите <ESC>');
                cur_time := Now;
                while end_tick > cur_time do
                begin
                    ch_key := ReadKey();
                    if SameKey(ch_key, ESC_KEY) then
                        do_exit := True;
                        info('Выход из цикла обработки');
                        break;
                    cur_time := Now;
                end;
            end
            else
                warning('Для выхода нажмите <Ctrl + C>');

            ENVIRONMENT.SetDateTimeValue('TICK_DT_STOP', Now);
            info('...Конец периода цикла обработки');
        end
    end
    else if sMode = RUN_MODE_DIAGNOSTIC then
         // Запуск регистратора в режиме диагностики источников данных
         DoDiagnostic(ENVIRONMENT.GetStrList('OBJECTS'))
    else
    begin
        warning(Format('Режим запуска регистратра <%s> не поддерживается системой', [sMode]));
        result := False;
        exit;
    end;
    result := True;
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
    for i := 0 to lObjects.Count - 1 do
    begin
        obj_properties := SettingsManager.BuildSection(lObjects[i]);
        obj := CreateDataCtrl(obj_properties);
        if obj <> Nil then
            result := result and obj.Diagnostic();
    end;
end;

end.

