unit dictionary;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    {
    Класс объектов строк для хранения в словаре
    }
    TObjString = class(TObject)
    public
       { Значение }
       Value: AnsiString;
    end;

    {
    Класс объектов даты-времени для хранения в словаре
    }
    TObjDateTime = class(TObject)
    public
       { Значение }
       Value: TDateTime;
    end;

    {
    Класс объектов списка строк для хранения в словаре
    }
    TObjStringList = class(TObject)
    public
       { Значение }
       Value: TStringList;
    end;

    {
    TStrDictionary - Простой словарь в качастве ключей у которого строки.
    }
    TStrDictionary = class(TStringList)
    public
       {Распечатать содержимое словаря}
       procedure PrintContent();
       {Проверка на существование переменной окружения с таким именем}
       function HasKey(sKey: AnsiString): Boolean;
       { Получить ключ по индексу }
       function GetKey(iIndex: Integer): AnsiString;
       { Список ключей }
       function GetKeys(): TStrings;
       function GetKeysStr(): AnsiString;
       {Получить объект по имени}
       function GetByName(sKey: AnsiString): TObject;

       { Проверка на тустой словарь }
       function IsEmpty(): Boolean;
       { Функция обновления словаря по другому словарю }
       function Update(Dictionary: TStrDictionary): Boolean;
       { Функция удаления элемента словаря }
       function DelItem(sKey: AnsiString): Boolean;

       { Добавить строку в словарь }
       function AddStrValue(sKey: AnsiString; sValue: AnsiString): LongInt;
       { Получить строку из словаря }
       function GetStrValue(sKey: AnsiString): AnsiString;
       { Установить строку в словарь }
       function SetStrValue(sKey: AnsiString; sValue: AnsiString): Boolean;

       { Добавить дату-время в словарь }
       function AddDateTimeValue(sKey: AnsiString; dtValue: TDateTime): LongInt;
       { Получить дату-время из словаря }
       function GetDateTimeValue(sKey: AnsiString): TDateTime;
       { Установить дату-время в словарь }
       function SetDateTimeValue(sKey: AnsiString; dtValue: TDateTime): Boolean;

       { Добавить список строк в словарь }
       function AddStrList(sKey: AnsiString; slValue: TStringList): LongInt;
       { Получить список строк из словаря }
       function GetStrList(sKey: AnsiString): TStringList;
       { Установить список строк в словарь }
       function SetStrList(sKey: AnsiString; slValue: TStringList): Boolean;

    end;

    {
    Класс объектов строк для хранения в словаре
    }
    TObjStrDictionary = class(TObject)
    public
       { Значение }
       Value: TStrDictionary;
    end;


implementation

uses
    log;

{
Печать содержимое словаря
}
procedure TStrDictionary.PrintContent();
var
  i: Integer;
  item_name: AnsiString;
  item_class: AnsiString;
  msg: AnsiString;
  item_obj: TObject;
begin
    try
        msg:=Format('Содержимое <%s : %s>:', [UnitName, ClassName]);
        service(msg);

        for i := 0 to GetCount-1 do
        begin
             item_name := Strings[i];
             item_obj := Objects[i];
             if item_obj <> Nil then
             begin
                item_class := item_obj.ClassName;
                if item_class = 'TObjString' then
                   item_class := (item_obj As TObjString).Value
                else
                   item_class := Format('<%s>', [item_class]);
             end
             else
                 item_class := '<Nil>';

             msg:=Format(#9'%s'#9'='#9'%s', [item_name, item_class]);
             service(msg);
        end;

    except
           fatal('Ошибка печати содержания окружения');
    end;
end;

{
Проверка на существование переменной окружения с таким именем
}
function TStrDictionary.HasKey(sKey: AnsiString): Boolean;
var
  idx: Integer;
begin
  idx := IndexOf(sKey);
  result := idx >= 0;
end;

{
Получить объект по имени.
}
function TStrDictionary.GetByName(sKey: AnsiString): TObject;
var
  idx: Integer;
begin
  idx := IndexOf(sKey);
  result := GetObject(idx);
end;

{
Проверка на пустой словарь
}
function TStrDictionary.IsEmpty(): Boolean;
begin
    result := Count = 0;
end;

{
Добавить строку
}
function TStrDictionary.AddStrValue(sKey: AnsiString; sValue: AnsiString): LongInt;
var
   obj: TObjString;
begin
     obj := TObjString.Create;
     obj.Value := sValue;
     result := AddObject(sKey, obj);
end;


{ Получить строку из словаря}
function TStrDictionary.GetStrValue(sKey: AnsiString): AnsiString;
var
   obj: TObjString;
begin
    result := '';
    obj := GetByName(sKey) As TObjString;
    if obj <> Nil then
       result := obj.Value;
end;

{
Установить строку
}
function TStrDictionary.SetStrValue(sKey: AnsiString; sValue: AnsiString): Boolean;
var
   obj: TObjString;
begin
     if not HasKey(sKey) then
     begin
        AddStrValue(sKey, sValue);
        result := True;
     end;

     obj := GetByName(sKey) As TObjString;
     obj.Value := sValue;
     result := True;
end;

{
Получить ключ по индексу
}
function TStrDictionary.GetKey(iIndex: Integer): AnsiString;
begin
     result := Strings[iIndex];
end;

{
Функция обновления словаря по другому словарю.
}
function TStrDictionary.Update(Dictionary: TStrDictionary): Boolean;
var
   i: Integer;
   key: AnsiString;
   obj: TObject;
begin
  if (Dictionary = Nil) or (Dictionary.IsEmpty) then
  begin
       result := False;
       exit;
  end;

  for i := 0 to Dictionary.Count - 1 do
  begin
       key := Dictionary.GetKey(i);
       obj := Dictionary.GetObject(i);
       if obj.ClassName = 'TObjString' then
          // Добавление строкового объекта
          AddStrValue(key, (obj As TObjString).Value)
       else
          // Добавление объекта
          AddObject(key, obj);
  end;
  result := True;
end;

{
Функция удаления элемента словаря
}
function TStrDictionary.DelItem(sKey: AnsiString): Boolean;
var
   idx: Integer;
begin
  idx := IndexOf(sKey);
  Delete(idx);
  result := True;
end;

{
Список ключей.
}
function TStrDictionary.GetKeys(): TStrings;
var
   keys: TStrings;
   i: Integer;
begin
  keys := TStrings.Create;
  for i := 0 to Count -1 do
      keys.Add(Strings[i]);
  result := keys;
end;

function TStrDictionary.GetKeysStr(): AnsiString;
var
   i: Integer;
begin
  result := '';
  for i := 0 to Count - 1 do
      result := result + ', ' + Format('''%s''', [Strings[i]]);
  result := '[' + result + ']';
end;

{
Добавить дату-время
}
function TStrDictionary.AddDateTimeValue(sKey: AnsiString; dtValue: TDateTime): LongInt;
var
   obj: TObjDateTime;
begin
     obj := TObjDateTime.Create;
     obj.Value := dtValue;
     result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetDateTimeValue(sKey: AnsiString): TDateTime;
var
   obj: TObjDateTime;
begin
    result := 0;
    obj := GetByName(sKey) As TObjDateTime;
    if obj <> Nil then
       result := obj.Value;
end;

{
Установить дату-время
}
function TStrDictionary.SetDateTimeValue(sKey: AnsiString; dtValue: TDateTime): Boolean;
var
   obj: TObjDateTime;
begin
     if not HasKey(sKey) then
     begin
        AddDateTimeValue(sKey, dtValue);
        result := True;
     end;

     obj := GetByName(sKey) As TObjDateTime;
     obj.Value := dtValue;
     result := True;
end;

{
Добавить список строк
}
function TStrDictionary.AddStrList(sKey: AnsiString; slValue: TStringList): LongInt;
var
   obj: TObjStringList;
begin
     obj := TObjStringList.Create;
     obj.Value := slValue;
     result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetStrList(sKey: AnsiString): TStringList;
var
   obj: TObjStringList;
begin
    result := Nil;
    obj := GetByName(sKey) As TObjStringList;
    if obj <> Nil then
       result := obj.Value;
end;

{
Установить дату-время
}
function TStrDictionary.SetStrList(sKey: AnsiString; slValue: TStringList): Boolean;
var
   obj: TObjStringList;
begin
     if not HasKey(sKey) then
     begin
        AddStrList(sKey, slValue);
        result := True;
     end;

     obj := GetByName(sKey) As TObjStringList;
     obj.Value := slValue;
     result := True;
end;

end.

