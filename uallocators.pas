unit uallocators;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { mnarena }

  mnarena = record
    data: PChar;
    capacity: uint64;
    count: uint64;
    is_dynamic: boolean;
    constructor Create(size: uint64);overload;
    procedure Free;
    function alloc(size: uint64): uint64;
    function get_item<T>(ind:UInt64):T;
    function add_item<T>(item:T):uint64;
  end;


implementation

{ mnarena }



constructor mnarena.Create(size: uint64);
begin
  Data := Getmem(size);
  self.capacity := size;
  self.Count := 0;
  is_dynamic := True;
end;


procedure mnarena.Free;
begin
  if is_dynamic then
    Freemem(Data);
  Data := nil;
  capacity := 0;
  Count := 0;
end;

function mnarena.alloc(size: uint64): uint64;
begin
  if self.Count + size > self.capacity then
  begin
    capacity := (self.capacity + size) * 2;
    Data := ReAllocMem(Data, capacity);
  end;
  Result := Count;
  Count := Count + size;
end;

function mnarena.get_item<T>(ind: UInt64): T;
type my_ptr=^T ;
 var ptr:^T;
begin
  ptr:=my_ptr(data+ind);
  result:= ptr^;
end;

function mnarena.add_item<T>(item: T): uint64;
 type mytype=^T;
begin
  result:=self.alloc(sizeof(T));
  mytype(data+result)^:=item;
end;

end.
