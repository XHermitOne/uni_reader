unit log;

{
Функции журналирования.

Цветовая расскраска сообщений в коммандной оболочке
производиться только под Linux.
Для Windows систем цветовая раскраска отключена.

Шаблон для использования в современных
командных оболочках и языках
программирования таков: \x1b[...m.
Это ESCAPE-последовательность,
где \x1b обозначает символ ESC
(десятичный ASCII код 27), а вместо "..."
подставляются значения из таблицы,
приведенной ниже, причем они могут
комбинироваться, тогда нужно их
перечислить через точку с запятой.

атрибуты
0 	нормальный режим
1 	жирный
4 	подчеркнутый
5 	мигающий
7 	инвертированные цвета
8 	невидимый

цвет текста
30 	черный
31 	красный
32 	зеленый
33 	желтый
34 	синий
35 	пурпурный
36 	голубой
37 	белый

цвет фона
40 	черный
41 	красный
42 	зеленый
43 	желтый
44 	синий
45 	пурпурный
46 	голубой
47 	белый

Версия: 0.0.3.2

ВНИМАНИЕ! Вывод сообщений под Linux проверять только в терминале.
Только он выводит корректно сообщения.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary, sysfunc;

const DEFAULT_LOG_FILENAME: AnsiString = 'uni_reader.log';

{ Цвета в консоли }
const RED_COLOR_TEXT: AnsiString = Chr($1b) + '[31;1m';       // red
const GREEN_COLOR_TEXT: AnsiString = Chr($1b) + '[32m';       // green
const YELLOW_COLOR_TEXT: AnsiString = Chr($1b) + '[33;1m';    // yellow
const BLUE_COLOR_TEXT: AnsiString = Chr($1b) + '[34m';        // blue
const PURPLE_COLOR_TEXT: AnsiString = Chr($1b) + '[35m';      // purple
const CYAN_COLOR_TEXT: AnsiString = Chr($1b) + '[36m';        // cyan
const WHITE_COLOR_TEXT: AnsiString = Chr($1b) + '[37m';       // white
const NORMAL_COLOR_TEXT: AnsiString = Chr($1b) + '[0m';       // normal

function GetDefaultEncoding(): AnsiString;
{Определить включен ли режим отладки}
function GetDebugMode(): Boolean;
{Определить включен ли режим журналирования}
function GetLogMode(): Boolean;

function EncodeUnicodeString(sTxt: AnsiString; sCodePage: AnsiString = 'utf-8'): AnsiString;
procedure PrintColorTxt(sTxt: AnsiString; sColor: AnsiString);

function OpenLog(sLogFileName: AnsiString = ''): boolean;
function CloseLog(): boolean;
function LogMsg(sMsg: AnsiString = ''): boolean;

procedure DebugMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure InfoMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure ErrorMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure WarningMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure FatalMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure ServiceMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);

var
    {
    Объявление глобального объекта файла журнала регистрации сообщений программы

    ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
    Переменные определенные в секции implementation являются статическими для
    модуля.
    }
    LOG_FILE: Text;
    IS_OPEN_LOG_FILE: Boolean = False;


implementation

uses
    config, filefunc;

{
Определить актуальную кодировку для вывода текста.
@return (Актуальная кодировка для вывода текста)
}
function GetDefaultEncoding(): AnsiString;
begin
    // writeln(DefaultSystemCodePage, ' ', CP_UTF8);
    result := 'utf-8';
end;

{
Определить включен ли режим отладки
}
function GetDebugMode(): Boolean;
begin
    result := ENVIRONMENT.HasKey('DEBUG_MODE');
    if not result then
       PrintColorTxt('Режим отладки отключен', YELLOW_COLOR_TEXT);
end;

{
Определить включен ли режим журналирования
}
function GetLogMode(): Boolean;
begin
    result := ENVIRONMENT.HasKey('LOG_MODE') and IS_OPEN_LOG_FILE;
    if not ENVIRONMENT.HasKey('LOG_MODE') then
       PrintColorTxt('Режим журналирования отключен', YELLOW_COLOR_TEXT);
end;

{
Перекодирование AnsiString строки в AnsiString.
@param (sTxt Текст в AnsiString)
@param (sCodePage Указание кодировки)
@return (Перекодированный текст)
}
function EncodeUnicodeString(sTxt: AnsiString; sCodePage: AnsiString): AnsiString;
begin
    result := '';
    if (sCodePage = 'utf-8') or (sCodePage = 'UTF-8') or (sCodePage = 'utf8') or (sCodePage = 'UTF8') then
    begin
        // ВНИМАНИЕ! Мы везде работаем с UTF-8 кодировкой
        // Поэтому перекодировать здесь не надо
        result := sTxt;
    end
    else
        writeln('Не поддерживаема кодировка <%s>', sCodePage);
end;

{
Печать цветового текста
@param (sTxt Печатаемый текст)
@param (sColor Дополнительное указание цветовой раскраски)
}
procedure PrintColorTxt(sTxt: AnsiString; sColor: AnsiString);
var
    str_txt: AnsiString;
begin
    str_txt := EncodeUnicodeString(sTxt, GetDefaultEncoding());
    // Для Windows систем цветовая раскраска отключена
    if IsOSLinux() then
        // Добавление цветовой раскраски для Linux систем
        str_txt := sColor + str_txt + NORMAL_COLOR_TEXT;
    WriteLn(str_txt);
end;

{
Инициализация файла лога.
@param (sLogFileName Имя файла лога.
        Если имя файла не определено, то пробуем его взять из оружения системы
        Ключ LOG_FILENAME)
}
function OpenLog(sLogFileName: AnsiString): Boolean;
begin
    result := False;

    // Если имя файла не определено, то пробуем его взять из оружения системы
    if (sLogFileName = '') and ENVIRONMENT.HasKey('LOG_FILENAME') then
       sLogFileName := (ENVIRONMENT.GetByName('LOG_FILENAME') As TObjString).Value;
    if sLogFileName = '' then
    begin
       WarningMsg('Не определено имя файла лога регистрации сообщений программы');
       exit;
    end;

    // Проверить наличие папки
    CreateDirPath(ExtractFileDir(sLogFileName));
    CreateEmptyFileIfNotExists(sLogFileName);

    try
       InfoMsg(Format('Файл регистрации сообщений программы <%s>', [sLogFileName]));
       AssignFile(LOG_FILE, sLogFileName);
       Append(LOG_FILE);
       if IOResult =0 then
          IS_OPEN_LOG_FILE := True;

       LogMsg('vvv Начало регистрации сообщений программы vvv');
       result := True;
    except
       CloseLog();
       FatalMsg('Ошибка открытия файла лога', True);
    end;
end;

{
Закрыть файл лога.
}
function CloseLog(): Boolean;
begin
    try
       if IS_OPEN_LOG_FILE then
       begin
         LogMsg('^^^ Окончание регистрации сообщений программы ^^^');
         CloseFile(LOG_FILE);
         IS_OPEN_LOG_FILE := False;
         result := True;
         exit;
       end;
    except
       on E: EInOutError do
          FatalMsg('Ошибка закрытия файла лога', True);
    end;
    result := False;
end;

{
Регистрация сообщения в файле лога.
@param (sMsg Регистрируемое сообщение)
@param (bForceLog Признак принудительной регистрации)
}
function LogMsg(sMsg: AnsiString = ''): Boolean;
var
    new_msg: AnsiString;
begin
     result := False;
     if not IS_OPEN_LOG_FILE then
        exit;

     new_msg := Format('%s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), sMsg]);
     try
        writeln(LOG_FILE, new_msg);
        result := True;
     except
        CloseLog();
        FatalMsg('Ошибка регистрации сообщения в лог файле', True);
     end;
end;

{
Вывести ОТЛАДОЧНУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure DebugMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      PrintColorTxt('DEBUG. ' + sMsg, BLUE_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('DEBUG. ' + sMsg);
end;

{
Вывести ТЕКСТОВУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure InfoMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      PrintColorTxt('INFO. ' + sMsg, GREEN_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('INFO. ' + sMsg);
end;

{
Вывести информацию об ОШИБКЕ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure ErrorMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      PrintColorTxt('ERROR. ' + sMsg, RED_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('ERROR. ' + sMsg);
end;

{
Вывести ПРЕДУПРЕЖДЕНИЕ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure WarningMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      PrintColorTxt('WARNING. ' + sMsg, YELLOW_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('WARNING. ' + sMsg);
end;


{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure FatalMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
var
    buf : array[0..511] of char;
    msg, except_msg: AnsiString;
begin
    msg := Format('FATAL. %s', [sMsg]);

    // StrPCopy(buf, DateTimeToStr(Now)+'. ');
    ExceptionErrorMessage(ExceptObject, ExceptAddr, @buf, SizeOf(buf));
    // StrCat(buf, #13#10);
    except_msg := buf;

    if (GetDebugMode()) or (bForcePrint) then
    begin
      PrintColorTxt(msg, RED_COLOR_TEXT);
      PrintColorTxt(except_msg, RED_COLOR_TEXT);
    end;
    if (GetLogMode()) or (bForceLog) then
    begin
       LogMsg(msg);
       LogMsg(except_msg);
    end;
end;

{
Вывести СЕРВИСНУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure ServiceMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      PrintColorTxt('SERVICE. ' + sMsg, CYAN_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('SERVICE. ' + sMsg);
end;


end.

