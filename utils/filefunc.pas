unit filefunc;

{
Функции работы с файлами.

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sysfunc, log, strfunc;

{ Определить папку домашней директории }
function GetHomeDir(): AnsiString;

{ Домашняя папка в Linux системах }
function GetOSLinuxHomeDir(): AnsiString;
{ Домашняя папка в Windows системах }
function GetOSWindowsHomeDir(): AnsiString;

{ Функция соединяет пути с учётом особенностей операционной системы }
function JoinPath(PathParts: Array Of String): AnsiString;

{ Функция разделяет путь на составляющие }
function SplitPath(sPath: AnsiString): TArrayOfString;

{ Создать весь путь папки }
function CreateDirPath(sPath: AnsiString): Boolean;

{ Создать пустой файл }
function CreateEmptyFile(sPath: AnsiString): Boolean;

{ Создать пустой файл если он не существует }
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;

implementation

{
Определить папку домашней директории
}
function GetHomeDir(): AnsiString;
begin
  result := '';
  if IsOSLinux() then
     result := GetOSLinuxHomeDir()
  else if IsOSWindows() then
     result := GetOSWindowsHomeDir()
     else
       warning(Format('Не поддерживаемая ОС <%s>', [GetOSType()]));
end;

{
Домашняя папка в Linux системах.
}
function GetOSLinuxHomeDir(): AnsiString;
begin
  result := '';
  {$IFDEF linux}
  result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

{
Домашняя папка в Windows системах.
}
function GetOSWindowsHomeDir(): AnsiString;
{$IFDEF windows}
var
    PIDL : PItemIDList;
    Folder : array[0..MAX_PATH] of Char;
    const CSIDL_PERSONAL = $0005;
{$ENDIF}
begin
    result := '';
    {$IFDEF windows}
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
    SHGetPathFromIDList(PIDL, Folder);
    result := Folder;
    {$ENDIF}
end;

{
Функция соединяет пути с учётом особенностей операционной системы.
}
function JoinPath(PathParts: Array Of String): AnsiString;
begin
     result := JoinStr(PathParts, PathDelim);
end;

{
Функция разделяет путь на составляющие.
}
function SplitPath(sPath: AnsiString): TArrayOfString;
begin
     result := SplitStr(sPath, PathDelim);
end;


{
Создать весь путь папки
}
function CreateDirPath(sPath: AnsiString): Boolean;
begin
  result := False;
  if not DirectoryExists(sPath) then
  begin
     info(Format('Создание папки <%s>', [sPath]));
     result := CreateDir(sPath)
  end;
end;

{
Создать пустой файл.
}
function CreateEmptyFile(sPath: AnsiString): Boolean;
var
    file_tmp: Text;
begin
    info(Format('Создание пустого файла <%s>', [sPath]));
    AssignFile(file_tmp, sPath);
    try
       Rewrite(file_tmp);
       Writeln(file_tmp, '');   //Remember AnsiStrings are case sensitive
       CloseFile(file_tmp);
       result := True;
    except
       result := False;
       CloseFile(file_tmp);
    end;
end;

{
Создать пустой файл если он не существует.
}
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;
begin
     result := False;
     if not FileExists(sPath) then
        result := CreateEmptyFile(sPath)
end;

end.

