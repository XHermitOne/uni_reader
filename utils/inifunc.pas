unit inifunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, INIFiles, StrUtils, dictionary, log;

type
    {
    TIniDictionary - Словарь словарей для хранения содержимого INI файла
    с разделением данных по секциям.
    }
    TIniDictionary = class(TStrDictionary)
    public

      constructor Create();
      constructor Create(sINIFileName: AnsiString);
      destructor Destroy; override;
      function Free: Boolean;

      { Загрузить содержимое INI файла }
      function LoadIniFile(sINIFileName: AnsiString): Boolean;
      { Получить значение параметра }
      function GetOptionValue(sSectionName: AnsiString; sOptionName: AnsiString): AnsiString;

    end;

implementation

constructor TIniDictionary.Create();
begin
     inherited Create;
end;

constructor TIniDictionary.Create(sINIFileName: AnsiString);
begin
     inherited Create;
     LoadIniFile(sINIFileName);
end;

destructor TIniDictionary.Destroy;
begin
     Free;
     inherited Destroy;
end;

{
Освободить всю память занимаемую объектом
}
function TIniDictionary.Free(): Boolean;
var
   i_section: Integer;
   section: TStrDictionary;
begin
     for i_section := 0 to Count - 1 do
     begin
          section := GetObject(i_section) As TStrDictionary;
          section.Free;
     end;
     inherited Free;
end;

{
Загрузить содержимое INI файла
}
function TIniDictionary.LoadIniFile(sINIFileName: AnsiString): Boolean;
var
   i_section, i_option, idx: Integer;
   ini_file: TIniFile;
   sections, options: TStringList;
   section_name, option, option_name, option_value: AnsiString;
   section_dict, option_dict: TStrDictionary;
begin
  result := False;
  if sIniFileName = '' then
  begin
     warning('Не определен INI файл для загрузки данных');
     exit;
  end;
  if not FileExists(sIniFileName) then
  begin
     warning(Format('Файл INI <%s> не найден', [sIniFileName]));
     exit;
  end;

  ini_file := TIniFile.Create(sIniFileName);

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
             section_dict := TStrDictionary.Create;

             options.Clear;
             ini_file.ReadSectionValues(section_name, options);
             for i_option :=0 to options.Count - 1 do
             begin
                  option := Trim(options[i_option]);
                  if AnsiStartsStr(option, ';') then
                     // Это коментарий обрабатывать не надо
                     continue;
                  idx := Pos('=', option);
                  option_name := Copy(option, 0, idx);
                  option_value := Copy(option, idx, Length(option)-idx);
                  section_dict.AddStrValue(option_name, option_value);
             end;
             AddObject(section_name, section_dict);
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

{ Получить значение параметра }
function TIniDictionary.GetOptionValue(sSectionName: AnsiString; sOptionName: AnsiString): AnsiString;
var
   section: TStrDictionary;
begin
     result := '';
     section := GetByName(sSectionName) As TStrDictionary;
     if section <> Nil then
        result := section.GetStrValue(sOptionName);
end;

end.

