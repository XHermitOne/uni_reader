unit strfunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils;

type
    TArrayOfString = Array Of String;
{
Разбивает строку с разделителями на части
и возвращает массив частей.
}
function SplitStr(sString: String; tdDelim: Char): TArrayOfString;

{ Объединение массива строк в одну строку }
function JoinStr(StringArray : Array Of String; tdDelim : Char): AnsiString;

{ Удалить обрамление кавычками одинарными и двойными из строки. }
function StripStr(sString: AnsiString; sChar: Char = ' '): AnsiString;

{ Распарсить строку, представляющую собой список строк }
function ParseStrList(sString: AnsiString): TArrayOfString;

{ Проверка является ли строка серилизованным списком }
function IsParseStrList(sString: AnsiString): Boolean;

implementation

{
Разбивает строку с разделителями на части
и возвращает массив частей.
}
function SplitStr(sString: String; tdDelim: Char): TArrayOfString;
var
  iCounter, iBegin: Integer;
begin
     if Length(sString) > 0 then
     begin
          //Include(tdDelim, #0);
          iBegin := 1;
          SetLength(Result, 0);

          for iCounter := 1 to Length(sString) + 1 do
          begin
               if (sString[iCounter] = tdDelim) then
               begin
                    SetLength(Result, Length(Result) + 1);
                    result[Length(Result) - 1] := Copy(sString, iBegin, iCounter - iBegin);
                    iBegin := iCounter + 1;
               end;
          end;//for
     end;//if
end;

{
Объединение массива строк в одну строку.
}
function JoinStr(StringArray: Array Of String; tdDelim: Char): AnsiString;
var
  i : Integer;
begin
  result := '';
  for i := Low(StringArray) to High(StringArray) do
    result := result + StringArray[i] + tdDelim;
  Delete(result, Length(result), 1);
end;

{
Удалить обрамление кавычками одинарными и двойными из строки.
}
function StripStr(sString: AnsiString; sChar: Char): AnsiString;
begin
     result := sString;
     if AnsiStartsStr(sString, sChar) then
        result := Copy(sString, 0, 1);
     if AnsiEndsStr(sString, sChar) then
        result := Copy(sString, Length(sString) - 1, 1);

     if AnsiStartsStr(sString, sChar) or AnsiEndsStr(sString, sChar) then
        result := StripStr(result, sChar);
end;

{
Распарсить строку, представляющую собой список строк
Например:
  [ aaaa, bbbb, cccc ] или ['aaa', 'bbb', 'ccc']
}
function ParseStrList(sString: AnsiString): TArrayOfString;
var
  i: Integer;
  result_list: Array Of String;
begin
     if AnsiStartsStr(sString, '[') then
        sString := Copy(sString, 0, 1);
     if AnsiStartsStr(sString, ']') then
        sString := Copy(sString, Length(sString) - 1, 1);
     result_list := SplitStr(sString, ',');
     for i := 0 to Length(result_list) - 1 do
     begin
          result_list[i] := StripStr(result_list[i], ' ');
          result_list[i] := StripStr(result_list[i], '''');
          result_list[i] := StripStr(result_list[i], '"');
     end;
     result := result_list;
end;

{
Проверка является ли строка серилизованным списком
}
function IsParseStrList(sString: AnsiString): Boolean;
begin
     result := AnsiStartsStr(sString, '[') and  AnsiEndsStr(sString, ']');
end;

end.

