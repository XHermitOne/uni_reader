unit settings;

{$mode objfpc}{$H+}

interface

uses
    {INIFiles = модуль который содержит класс для работы с INI-файлами}
    Classes, SysUtils, INIFiles, StrUtils,
    log, config, inifunc, dictionary, strfunc;

const DEFAULT_SETTINGS_INI_FILENAME: AnsiString = 'settings.ini';

type
    {
    TICSettingsManager - Менеджер управления настройками программы
    }

    TICSettingsManager = class(TObject)
    public
      { Полное наименование INI файла }
      IniFileName: AnsiString;
      { Содержимое INI файла в виде словаря словарей (разложено по секциям)}
      Content: TIniDictionary;

      constructor Create;
      destructor Destroy; override;

      {Генерация имени настроечного INI файла}
      function GenIniFileName(): AnsiString;
      { Вывод на экран текущих настроек для отладки. }
      procedure PrintSettings();
      { Загрузка параметров изи INI файла }
      function LoadSettings(sINIFileName: AnsiString): Boolean;
      { Проверка существования файла настройки}
      function ExistsIniFile(sINIFileName: AnsiString): Boolean;
      { Собрать полное описание секции с учетом ключа parent }
      function BuildSection(sSectionName: Ansistring): TStrDictionary;

    end;

var
   SETTINGS_MANAGER: TICSettingsManager;


implementation

uses
    filefunc;

constructor TICSettingsManager.Create;
begin
     inherited Create;
     Content := TIniDictionary.Create;
end;

destructor TICSettingsManager.Destroy;
begin
     Content.Free;
     inherited Destroy;
end;

{
Генерация имени настроечного INI файла
}
function TICSettingsManager.GenIniFileName(): AnsiString;
var
  cur_path: AnsiString;
begin
     cur_path := ExtractFileDir(ParamStr(0));

     IniFileName := JoinPath([cur_path, DEFAULT_SETTINGS_INI_FILENAME]);

     debug(Format('Файл настроек: <%s>', [IniFileName]));

     result := IniFileName;
end;

{
Вывод на экран текущих настроек для отладки.
}
procedure TICSettingsManager.PrintSettings();
var
   i_section, i_option: Integer;
   ini_file: TIniFile;
   sections: TStringList;
   section_name: AnsiString;
   options: TStringList;
   option: AnsiString;
begin
     if IniFileName = '' then
     begin
        warning('Не определен INI файл настроек для отображения');
        exit;
     end;
     if not FileExists(IniFileName) then
     begin
        warning(Format('Файл настроек программы <%s> не найден', [IniFileName]));
        exit;
     end;

     ini_file := TIniFile.Create(IniFileName);
     service(Format('Файл настроек программы <%s>:', [IniFileName]));
     // ВНИМАНИЕ! Перед использованием списков строк в функции
     // надо их создать/выделить под них память
     sections := TStringList.Create;
     options := TStringList.Create;
     try
        try
           ini_file.ReadSections(sections);
           for i_section :=0 to sections.Count - 1 do
           begin
                section_name := sections[i_section];
                service(Format('[%s]', [section_name]));

                options.Clear;
                ini_file.ReadSectionValues(section_name, options);
                for i_option :=0 to options.Count - 1 do
                begin
                     option := options[i_option];
                     if AnsiStartsStr(option, ';') then
                        // Это коментарий обрабатывать не надо
                        continue;
                     service(Format(#9'%s', [option]));
                end;
           end;
        finally
               ini_file.Free;
        end;
     except
           fatal('Ошибка печати настроек программы');
     end;
     // ВНИМАНИЕ! В конце обязательно освободить память
     options.Free;
     sections.Free;
end;

{
Загрузка настроек из INI файла.
@param (sINIFileName Полное имя конфигурационного файла.
        Если не определено, то генерируется.)
@return: (True - загрузка параметров прошла успешно,
          False - загрузка не прошла по какой-либо причине)
}
function TICSettingsManager.LoadSettings(sINIFileName: AnsiString): Boolean;
begin
    if sINIFileName = '' then
        sINIFileName := GenINIFileName();

    if FileExists(sINIFileName) then
    begin
       Content.LoadIniFile(sIniFileName);
       if not Content.IsEmpty then
       begin
            // Прописать настройки в окружении
            ENVIRONMENT.AddObject('SETTINGS', self);
            result := True;
       end
       else
           warning(Format('Не определены настройки в INI файле <%s>' , [sIniFileName]));
    end;
    result := False;
end;

{
Собрать полное описание секции с учетом ключа parent.
Через ключ parent можно наследовать описание секции.
@param (sSectionName Наименование запрашиваемой секции)
@return (Словарь секции дополненный переменными из секции указанной в parent.
    Сборка данных производиться рекурсивно.)
}
function TICSettingsManager.BuildSection(sSectionName: Ansistring): TStrDictionary;
var
   section, parent_section, result_section: TStrDictionary;
   i: Integer;
   parent_section_list: Array Of String;
   parent_section_name: AnsiString;
begin
    section := TStrDictionary.Create;
    if Content.HasKey(sSectionName) then
       section.Update((Content.GetByName(sSectionName) As TObjStrDictionary).Value)
    else
       section.AddStrValue('name', sSectionName);

    if not section.HasKey('parent') then
    begin
        result := section;
        exit;
    end
    else if section.GetStrValue('parent') = '' then
    begin
        section.DelItem('parent');
        result := section;
        exit;
    end
    else if not Content.HasKey(section.GetStrValue('parent')) then
    begin
        warning(Format('Запрашиваемая секция <%s> как родительская для <%s> не найдена', [section.GetStrValue('parent'), sSectionName]));
        section.DelItem('parent');
        result := section;
        exit;
    end
    else
        if IsParseStrList(section.GetStrValue('parent')) then
        begin
           // Список имен
           parent_section_list := ParseStrList(section.GetStrValue('parent'));
           result_section := TStrDictionary.Create;
           for i := 0 to Length(parent_section_list) - 1 do
           begin
               parent_section_name := parent_section_list[i];
               parent_section := BuildSection(section.GetStrValue('parent'));
               result_section.Update(parent_section);
           end;
           result_section.Update(section);
           result_section.DelItem('parent');
           result := result_section;
           exit;
        end
        else
        begin
            // Имя родительской секции
            parent_section := BuildSection(section.GetStrValue('parent'));
            parent_section.Update(section);
            parent_section.DelItem('parent');
            result := parent_section;
            exit;
        end;

    result := section;
end;

//def saveSettings(self, sINIFileName=None):
//    """
//    Сохранение настрек в INI файле.
//    @type sINIFileName: C{string}
//    @param sINIFileName: Полное имя конфигурационного файла.
//        Если None, то генерируется.
//    @return: True - запись параметров прошла успешно,
//        False - запись не прошла по какой-либо причине.
//    """
//    if sINIFileName is None:
//        sINIFileName = genINIFileName()
//
//    settings = dict()
//    # Сохранение настроечных переменных в словаре настроек
//
//    log.info('SAVE SETTINGS')
//    if utils.isDebugMode():
//        printSettings(settings)
//
//    return ini.Dict2INI(settings, sINIFileName)

{
Проверка существования файла настройки.
@param (sINIFileName Полное имя конфигурационного файла.
        Если None, то генерируется )
}
function TICSettingsManager.ExistsIniFile(sINIFileName: AnsiString): Boolean;
begin
    if sINIFileName = '' then
        sINIFileName := GenIniFileName();

    result := FileExists(sINIFileName);
end;


var
  ini_filename: AnsiString;

begin
  SETTINGS_MANAGER := TICSettingsManager.Create;
  ini_filename := SETTINGS_MANAGER.GenIniFileName();
  ENVIRONMENT.AddStrValue('SETTINGS_FILENAME', ini_filename);
  SETTINGS_MANAGER.PrintSettings;
end.
