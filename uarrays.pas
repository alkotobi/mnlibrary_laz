unit uarrays;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

const
  MNARRAY_INIT_CAP = 100;

type

  { mnarray }

  mnarray<T> = record
    Data: tarray<T>;
    capacity: integer;
    Count: integer;
    procedure init; overload;
    procedure init(capacity: integer); overload;
    procedure init_from_mnarray(arr:mnarray<T>);
    procedure add(item: T);
    procedure add_array(arr:mnarray<T> );
    function item_at_ind(ind:integer):T;
    procedure delete_item_at_ind(ind:integer);
    procedure clear;

  end;

  const null_array:mnarray<integer> =(data:nil;capacity:-1;count:-1);


procedure mnarray_init<T>(var arr: mnarray<T>); overload;
procedure mnarray_init<T>(var arr: mnarray<T>; capacity: integer); overload;
procedure mnarray_add<T>(var arr: mnarray<T>; item: T);
function mnarray_item_at_ind<T>(var arr: mnarray<T>; index: integer): T;
procedure mnarray_delete_item_at_index<T>(var arr: mnarray<T>; index: integer);


type
  mnamed<T> = record
    item1: string;
    item2: T;
  end;

  mnstring_array = mnarray<string>;

procedure mnstring_array_init(var arr: mnstring_array); overload;
procedure mnstring_array_init(var arr: mnstring_array; capacity: integer); overload;
procedure mnstring_array_add(var arr: mnstring_array; item: string);
function mnstring_array_item_at_ind(var arr: mnstring_array; index: integer): string;
procedure mnstring_array_delete_item_at_index(var arr: mnstring_array; index: integer);
procedure mnstring_array_init_from_tstrings(strs: TStrings;
  var string_array: mnstring_array);

implementation

procedure mnstring_array_init(var arr: mnstring_array);
begin
  mnarray_init<string>(arr);
end;

procedure mnstring_array_init(var arr: mnstring_array; capacity: integer);
begin
  mnarray_init<string>(arr, capacity);
end;

procedure mnstring_array_add(var arr: mnstring_array; item: string);
begin
  mnarray_add<string>(arr, item);
end;

function mnstring_array_item_at_ind(var arr: mnstring_array; index: integer): string;
begin
  Result := mnarray_item_at_ind<string>(arr, index);
end;

procedure mnstring_array_delete_item_at_index(var arr: mnstring_array; index: integer);
begin
  mnarray_delete_item_at_index<string>(arr, index);
end;

procedure mnstring_array_init_from_tstrings(strs: TStrings;
  var string_array: mnstring_array);
var
  i: integer;
begin
  mnstring_array_init(string_array, strs.Count);
  for i := 0 to strs.Count - 1 do
  begin
    string_array.Data[i] := strs.Strings[i];
  end;
  string_array.capacity := strs.Count;
  string_array.Count := strs.Count;
end;

procedure mnarray_init<T>(var arr: mnarray<T>);
begin
  mnarray_init<T>(arr, MNARRAY_INIT_CAP);
end;

procedure mnarray_init<T>(var arr: mnarray<T>; capacity: integer);
begin
  arr.capacity := capacity;
  SetLength(arr.Data, capacity);
  arr.Count := 0;
end;

procedure mnarray_add<T>(var arr: mnarray<T>; item: T);
begin
  if arr.Count = arr.capacity then
  begin
    arr.capacity := arr.capacity * 2;
    SetLength(arr.Data, arr.capacity);
  end;
  arr.Data[arr.Count] := item;
  arr.Count := arr.Count + 1;
end;

function mnarray_item_at_ind<T>(var arr: mnarray<T>; index: integer): T;
begin
  Result := arr.Data[index];
end;

procedure mnarray_delete_item_at_index<T>(var arr: mnarray<T>; index: integer);
var
  i: integer;
begin
  if index = arr.Count - 1 then
  begin
    arr.Count := arr.Count - 1;
    exit;
  end;
  for i := index to arr.Count - 2 do
  begin
    arr.Data[i] := arr.Data[i + 1];
  end;
  arr.Count := arr.Count - 1;
end;

{ mnarray }

procedure mnarray<T>.init;
begin
  mnarray_init<T>(self);
end;

procedure mnarray<T>.init(capacity: integer);
begin
  mnarray_init<T>(self, capacity);
end;

procedure mnarray<T>.init_from_mnarray(arr: mnarray<T>);
 var i:integer;
begin
  mnarray_init<T>(self, arr.capacity);
  for i:= 0 to arr.COUNT -1 do
  begin
    self.Data[i]:=arr.data[i];
  end;
  self.Count:=arr.Count;
end;

procedure mnarray<T>.add(item: T);
begin
  mnarray_add<T>(Self, item);
end;

procedure mnarray<T>.add_array(arr: mnarray<T>);
 var i:integer;
begin
  for i:= 0 to arr.Count-1 do
  begin
     self.add(arr.data[i]);
  end;
end;

function mnarray<T>.item_at_ind(ind: integer):T;
begin
  result := mnarray_item_at_ind<T>(self,ind);
end;

procedure mnarray<T>.delete_item_at_ind(ind: integer);
begin
  mnarray_delete_item_at_index<T>(self,ind);
end;

procedure mnarray<T>.clear;
begin
  self.capacity:=MNARRAY_INIT_CAP;
  self.Count:=0;
  SetLength(self.data,0);
  SetLength(self.data,MNARRAY_INIT_CAP);
end;




end.
