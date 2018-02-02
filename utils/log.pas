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

Версия: 0.0.2.1
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

function get_default_encoding(): AnsiString;
{Определить включен ли режим отладки}
function get_debug_mode(): Boolean;
{Определить включен ли режим журналирования}
function get_log_mode(): Boolean;

function encode_unicode_string(sTxt: AnsiString; sCodePage: AnsiString = 'utf-8'): AnsiString;
procedure print_color_txt(sTxt: AnsiString; sColor: AnsiString);

function open_log(sLogFileName: AnsiString = ''): boolean;
function close_log(): boolean;
function log_msg(sMsg: AnsiString = ''): boolean;

procedure debug(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure info(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure error(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure warning(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure fatal(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
procedure service(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);

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
function get_default_encoding(): AnsiString;
begin
    // writeln(DefaultSystemCodePage, ' ', CP_UTF8);
    result := 'utf-8';
end;

{
Определить включен ли режим отладки
}
function get_debug_mode(): Boolean;
begin
    result := ENVIRONMENT.HasKey('DEBUG_MODE');
end;

{
Определить включен ли режим журналирования
}
function get_log_mode(): Boolean;
begin
    result := ENVIRONMENT.HasKey('LOG_MODE');
end;

{
Перекодирование AnsiString строки в AnsiString.
@param (sTxt Текст в AnsiString)
@param (sCodePage Указание кодировки)
@return (Перекодированный текст)
}
function encode_unicode_string(sTxt: AnsiString; sCodePage: AnsiString): AnsiString;
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
procedure print_color_txt(sTxt: AnsiString; sColor: AnsiString);
var
    str_txt: AnsiString;
begin
    str_txt := encode_unicode_string(sTxt, get_default_encoding());
    // Для Windows систем цветовая раскраска отключена
    if IsOSLinux() then
        // Добавление цветовой раскраски для Linux систем
        str_txt := sColor + str_txt + NORMAL_COLOR_TEXT;
    writeln(str_txt);
end;

{
Инициализация файла лога.
@param (sLogFileName Имя файла лога.
        Если имя файла не определено, то пробуем его взять из оружения системы
        Ключ LOG_FILENAME)
}
function open_log(sLogFileName: AnsiString): Boolean;
begin
    result := False;

    // Если имя файла не определено, то пробуем его взять из оружения системы
    if (sLogFileName = '') and ENVIRONMENT.HasKey('LOG_FILENAME') then
       sLogFileName := (ENVIRONMENT.GetByName('LOG_FILENAME') As TObjString).Value;
    if sLogFileName = '' then
    begin
       warning('Не определено имя файла лога регистрации сообщений программы');
       exit;
    end;

    // Проверить наличие папки
    CreateDirPath(ExtractFileDir(sLogFileName));
    CreateEmptyFileIfNotExists(sLogFileName);

    try
       info(Format('Файл регистрации сообщений программы <%s>', [sLogFileName]));
       AssignFile(LOG_FILE, sLogFileName);
       Append(LOG_FILE);
       if IOResult =0 then
          IS_OPEN_LOG_FILE := True;

       log_msg('vvv Начало регистрации сообщений программы vvv');
       result := True;
    except
       close_log();
       fatal('Ошибка открытия файла лога', True);
    end;
end;

{
Закрыть файл лога.
}
function close_log(): Boolean;
begin
    try
       if IS_OPEN_LOG_FILE then
       begin
         log_msg('^^^ Окончание регистрации сообщений программы ^^^');
         CloseFile(LOG_FILE);
         IS_OPEN_LOG_FILE := False;
         result := True;
       end;
    except
       on E: EInOutError do
          fatal('Ошибка закрытия файла лога', True);
    end;
    result := False;
end;

{
Регистрация сообщения в файле лога.
@param (sMsg Регистрируемое сообщение)
@param (bForceLog Признак принудительной регистрации)
}
function log_msg(sMsg: AnsiString = ''): Boolean;
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
        close_log();
        fatal('Ошибка регистрации сообщения в лог файле', True);
     end;
end;

{
Вывести ОТЛАДОЧНУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure debug(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (get_debug_mode()) or (bForcePrint) then
       print_color_txt('DEBUG. ' + sMsg, BLUE_COLOR_TEXT);
    if (get_log_mode()) or (bForceLog) then
       log_msg('DEBUG. ' + sMsg);
end;

{
Вывести ТЕКСТОВУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure info(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (get_debug_mode()) or (bForcePrint) then
       print_color_txt('INFO. ' + sMsg, GREEN_COLOR_TEXT);
    if (get_log_mode()) or (bForceLog) then
       log_msg('INFO. ' + sMsg);
end;

{
Вывести информацию об ОШИБКЕ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure error(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (get_debug_mode()) or (bForcePrint) then
       print_color_txt('ERROR. ' + sMsg, RED_COLOR_TEXT);
    if (get_log_mode()) or (bForceLog) then
       log_msg('ERROR. ' + sMsg);
end;

{
Вывести ПРЕДУПРЕЖДЕНИЕ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure warning(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (get_debug_mode()) or (bForcePrint) then
       print_color_txt('WARNING. ' + sMsg, YELLOW_COLOR_TEXT);
    if (get_log_mode()) or (bForceLog) then
       log_msg('WARNING. ' + sMsg);
end;


{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure fatal(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
var
    buf : array[0..511] of char;
    msg, except_msg: AnsiString;
begin
    msg := Format('FATAL. %s', [sMsg]);

    // StrPCopy(buf, DateTimeToStr(Now)+'. ');
    ExceptionErrorMessage(ExceptObject, ExceptAddr, @buf, SizeOf(buf));
    // StrCat(buf, #13#10);
    except_msg := buf;

    if (get_debug_mode()) or (bForcePrint) then
    begin
       print_color_txt(msg, RED_COLOR_TEXT);
       print_color_txt(except_msg, RED_COLOR_TEXT);
    end;
    if (get_log_mode()) or (bForceLog) then
    begin
       log_msg(msg);
       log_msg(except_msg);
    end;
end;

{
Вывести СЕРВИСНУЮ информацию.
@param (sMsg Текстовое сообщение)
@param (bForcePrint Принудительно вывести на экран)
@param (bForceLog Принудительно записать в журнале)
}
procedure service(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (get_debug_mode()) or (bForcePrint) then
       print_color_txt('SERVICE. ' + sMsg, CYAN_COLOR_TEXT);
    if (get_log_mode()) or (bForceLog) then
       log_msg('SERVICE. ' + sMsg);
end;


end.

