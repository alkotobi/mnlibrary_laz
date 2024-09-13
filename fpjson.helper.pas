unit fpjson.helper;

{$mode objfpc}{$H+}

interface

{$ifndef ver3_0}
{ there still seems to be a bug in trunk regarding overloads with non generic
  methods, so disable them for now }
{.$define enable_gen_funcs}
{$endif}

uses
  fpjson, fpjsonrtti, typinfo;

type
  TJSONStreamerHelper = class helper for TJSONStreamer
  private
    function StreamArray(aPointer: Pointer; aTypeInfo: PTypeInfo): TJSONArray;
    function StreamField(aPointer: Pointer; aTypeInfo: PTypeInfo; const aName: String): TJSONData;
  public
    function RecordToJSON(aRecord: Pointer; aTypeInfo: PTypeInfo): TJSONObject;

    generic function RecordToJSON<T>(constref aRecord: T): TJSONObject; inline;


    function RecordToJSONString(aRecord: Pointer; aTypeInfo: PTypeInfo): TJSONStringType;

    generic function RecordToJSONString<T>(constref aRecord: T): TJSONStringType; inline;

  end;

  TJSONDeStreamerHelper = class helper for TJSONDeStreamer
  private
    procedure RestoreArray(aPointer: Pointer; aTypeInfo: PTypeInfo; aArrayData: TJSONArray);
    procedure DoRestoreField(aPointer: Pointer; aTypeInfo: PTypeInfo; aFieldData: TJSONData; const aName: String);
    procedure RestoreField(aPointer: Pointer; aTypeInfo: PTypeInfo; aFieldData: TJSONData; const aName: String);
  public
    procedure JSONToRecord(const aJSON: TJSONStringType; aRecord: Pointer; aTypeInfo: PTypeInfo);
    procedure JSONToRecord(const aJSON: TJSONObject; aRecord: Pointer; aTypeInfo: PTypeInfo);


    generic procedure JSONToRecord<T>(const aJSON: TJSONStringType; out aRecord: T);
    generic procedure JSONToRecord<T>(const aJSON: TJSONObject; out aRecord: T);

  end;

implementation

uses
  SysUtils;

resourcestring
  SErrUnknownFieldKind        = 'Unknown field kind for field: %s';
  SErrUnsupportedFieldKind    = 'Unsupported field kind for field: %s';
  SErrArrayCountMismatch      = 'Array count does not match; expected count of %d, but got %d';
  SErrUnSupportedEnumDataType = 'Unsupported JSON type for enumerated property "%s" : "%s"';
  SErrUnsupportedJSONType     = 'Cannot destream object from JSON data of type "%s"';

type
  TSet = set of 0..31;

{ TJSONDeStreamerHelper }

procedure TJSONDeStreamerHelper.RestoreArray(aPointer: Pointer;
  aTypeInfo: PTypeInfo; aArrayData: TJSONArray);
var
  td: PTypeData;
  elsize: LongInt;
  i: LongInt;
begin
  { for now we stream the array as a flat array even if it's a multi dim one }
  td := GetTypeData(aTypeInfo);
  elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
  if td^.ArrayData.ElCount <> aArrayData.Count then
    Error(sErrArrayCountMismatch, [td^.ArrayData.ElCount, aArrayData.Count]);
  for i := 0 to td^.ArrayData.ElCount - 1 do begin
    DoRestoreField(PByte(aPointer) + elsize * i, td^.ArrayData.ElType, aArrayData.Items[i], 'Array ' + IntToStr(i));
  end;
end;

procedure TJSONDeStreamerHelper.DoRestoreField(aPointer: Pointer;
  aTypeInfo: PTypeInfo; aFieldData: TJSONData; const aName: String);

var
  td: PTypeData;

  procedure SetIntegerField;
  begin
    case td^.OrdType of
      otSByte: PInt8(aPointer)^ := aFieldData.AsInteger;
      otUByte: PUInt8(aPointer)^ := aFieldData.AsInteger;
      otSWord: PInt16(aPointer)^ := aFieldData.AsInteger;
      otUWord: PUInt16(aPointer)^ := aFieldData.AsInteger;
      otSLong: PInt32(aPointer)^ := aFieldData.AsInteger;
      otULong: PUInt32(aPointer)^ := aFieldData.AsInteger;
//{$ifndef ver3_0}
//      otSQWord: PInt64(aPointer)^ := aFieldData.AsInt64;
//      otUQWord: PUInt64(aPointer)^ := aFieldData.AsQWord;
//{$endif}
    end;
  end;

  procedure SetOrdValue(aValue: LongInt);
  begin
    case td^.OrdType of
      otSByte: PInt8(aPointer)^ := aValue;
      otUByte: PUInt8(aPointer)^ := aValue;
      otSWord: PInt16(aPointer)^ := aValue;
      otUWord: PUInt16(aPointer)^ := aValue;
      otSLong: PInt32(aPointer)^ := aValue;
      otULong: PUInt32(aPointer)^ := aValue;
//{$ifndef ver3_0}
//      otSQWord: PInt64(aPointer)^ := aValue;
//      otUQWord: PUInt64(aPointer)^ := aValue;
//{$endif}
    end;
  end;

  procedure SetFloatField(aValue: Extended);
  begin
    case td^.FloatType of
      ftSingle: PSingle(aPointer)^ := aValue;
      ftDouble: PDouble(aPointer)^ := aValue;
      ftExtended: PExtended(aPointer)^ := aValue;
      ftComp: PComp(aPointer)^ := Comp(aValue);
      ftCurr: PCurrency(aPointer)^ := Currency(aValue);
    end;
  end;

var
  js: TJSONStringType;
  a: TJSONArray;
  s, i, j: LongInt;
begin
  td := GetTypeData(aTypeInfo);
  case aTypeInfo^.Kind of
    tkUnknown :
      Error(SErrUnknownFieldKind, [aName]);
    tkInteger :
      SetIntegerField;
    tkInt64 :
      PInt64(aPointer)^ := aFieldData.AsInt64;
    tkEnumeration :
      begin
        if aFieldData.JSONType = jtNumber then
          i := aFieldData.AsInteger
        else if aFieldData.JSONType = jtString then
          i := GetEnumValue(aTypeInfo, aFieldData.AsString)
        else
          Error(SErrUnSupportedEnumDataType,[aName, JSONTypeName(aFieldData.JSONType)]);
        SetOrdValue(i);
      end;
    tkFloat :
      begin
        if (aTypeInfo = TypeInfo(TDateTime)) and (aFieldData.JSONType = jtString) then
          SetFloatField(ExtractDateTime(aFieldData.AsString))
        else
          SetFloatField(aFieldData.AsFloat)
      end;
    tkSet :
      if aFieldData.JSONType = jtString then
        SetOrdValue(StringToSet(aTypeInfo, aFieldData.AsString))
      else if aFieldData.JSONType = jtArray then begin
        a := aFieldData as TJSONArray;
        s := 0;
        for i := 0 to A.Count-1 do begin
          if a.Types[i] = jtNumber then
            j := a.Integers[i]
          else
            j := GetEnumValue(td^.CompType, a.Strings[i]);
          TSet(S) := TSet(S) + [j];
        end;
        SetOrdValue(s);
      end;
    tkChar:
      begin
        js := aFieldData.AsString;
        if js <> '' then
          SetOrdValue(Ord(js[1]));
      end;
    tkSString:
      PShortString(aPointer)^ := aFieldData.AsString;
    tkLString,
    tkAString:
      PAnsiString(aPointer)^ := aFieldData.AsString;
    tkWString :
      PWideString(aPointer)^ := aFieldData.AsUnicodeString;
    tkVariant:
      PVariant(aPointer)^ := JSONToVariant(aFieldData);
    tkWChar :
      begin
        js := aFieldData.AsString;
        if js <> '' then
          SetOrdValue(Ord(js[1]));
      end;
    tkBool :
      SetOrdValue(Ord(aFieldData.AsBoolean));
    tkQWord :
      PQWord(aPointer)^ := aFieldData.AsQWord;
    tkArray:
      RestoreArray(aPointer, aTypeInfo, aFieldData as TJSONArray);
    tkRecord:
      JSONToRecord(aFieldData as TJSONObject, aPointer, aTypeInfo);
    tkClass,
    tkObject,
    tkInterface,
    tkDynArray,
    tkInterfaceRaw,
    tkProcVar,
    tkMethod :
      Error(SErrUnsupportedFieldKind, [aName]);
    tkUString :
      PUnicodeString(aPointer)^ := aFieldData.AsUnicodeString;
    tkUChar:
      begin
        js := aFieldData.AsString;
        if js <> '' then
          SetOrdValue(Ord(js[1]));
      end;
  end;
end;

procedure TJSONDeStreamerHelper.RestoreField(aPointer: Pointer;
  aTypeInfo: PTypeInfo; aFieldData: TJSONData; const aName: String);
begin
  try
    DoRestoreField(aPointer, aTypeInfo, aFieldData, aName);
  except
    if not (jdoIgnorePropertyErrors in Options) then
      raise;
  end;
end;

procedure TJSONDeStreamerHelper.JSONToRecord(const aJSON: TJSONStringType;
  aRecord: Pointer; aTypeInfo: PTypeInfo);
var
  d: TJSONData;

begin
  d := ObjectFromString(aJSON);
  try
    If d.JSONType = jtObject then
      JSONToRecord(d as TJSONObject, aRecord, aTypeInfo)
    else
      Error(SErrUnsupportedJSONType, [JSONTypeName(d.JSONType)]);
  finally
    d.Free;
  end;
end;

procedure TJSONDeStreamerHelper.JSONToRecord(const aJSON: TJSONObject;
  aRecord: Pointer; aTypeInfo: PTypeInfo);
var
  td: PTypeData;
  mf: PManagedField;
  i, j: LongInt;
  name: TJSONStringType;
begin
  if not Assigned(aJSON) or not Assigned(aRecord) or not Assigned(aTypeInfo) then
    Exit;
  if aTypeInfo^.Kind <> tkRecord then
    Exit;

  td := GetTypeData(aTypeInfo);

  { ToDo: check for targets with FPC_REQUIRES_PROPER_ALIGNMENT set }
  mf := PManagedField(PByte(@td^.ManagedFldCount) + SizeOf(td^.ManagedFldCount));
//{$else}
//  mf := PManagedField(AlignTypeData(PByte(@td^.TotalFieldCount) + SizeOf(td^.TotalFieldCount)));
//{$endif}
  for i := 0 to {$ifdef ver3_0}td^.ManagedFldCount{$else}td^.TotalFieldCount{$endif} - 1 do begin
    name := 'Field' + IntToStr(i + 1);
    j := aJSON.IndexOfName(name, jdoCaseInsensitive in Options);
    if j >= 0 then
      RestoreField(PByte(aRecord) + mf^.FldOffset, mf^.TypeRef, aJSON.Items[j], name);
    Inc(mf);
  end;
end;


generic procedure TJSONDeStreamerHelper.JSONToRecord<T>(const aJSON: TJSONStringType; out aRecord: T);
begin
  JSONToRecord(aJSON, @aRecord, TypeInfo(aRecord));
end;

generic procedure TJSONDeStreamerHelper.JSONToRecord<T>(const aJSON: TJSONObject; out aRecord: T);
begin
  JSONToRecord(aJSON, @aRecord, TypeInfo(aRecord));
end;


{ TJSONStreamerHelper }

function TJSONStreamerHelper.StreamArray(aPointer: Pointer; aTypeInfo: PTypeInfo
  ): TJSONArray;
var
  td: PTypeData;
  elsize: LongInt;
  ad: TJSONData;
  i: LongInt;
begin
  Result := TJSONArray.Create;
  { for now we stream the array as a flat array even if it's a multi dim one }
  td := GetTypeData(aTypeInfo);
  elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
  for i := 0 to td^.ArrayData.ElCount - 1 do begin
    try
      ad := StreamField(PByte(aPointer) + elsize * i, td^.ArrayData.ElType, 'Array ' + IntToStr(i));
      Result.Add(ad);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TJSONStreamerHelper.StreamField(aPointer: Pointer;
  aTypeInfo: PTypeInfo; const aName: String): TJSONData;

var
  td: PTypeData;

  function GetIntegerField: TJSONData;
  begin
    case td^.OrdType of
      otSByte:
        Result := TJSONIntegerNumber.Create(PInt8(aPointer)^);
      otUByte:
        Result := TJSONIntegerNumber.Create(PUInt8(aPointer)^);
      otSWord:
        Result := TJSONIntegerNumber.Create(PInt16(aPointer)^);
      otUWord:
        Result := TJSONIntegerNumber.Create(PUInt16(aPointer)^);
      otSLong:
        Result := TJSONIntegerNumber.Create(PInt32(aPointer)^);
      otULong:
        Result := TJSONIntegerNumber.Create(PUInt32(aPointer)^);
//
//      otSQWord:
//        Result := TJSONInt64Number.Create(PInt64(aPointer)^);
//      otUQWord:
//        Result := TJSONQWordNumber.Create(PUInt64(aPointer)^);

    end;
  end;

  function GetOrdValue: LongInt;
  begin
    case td^.OrdType of
      otSByte:
        Result := PInt8(aPointer)^;
      otUByte:
        Result := PUInt8(aPointer)^;
      otSWord:
        Result := PInt16(aPointer)^;
      otUWord:
        Result := PUInt16(aPointer)^;
      otSLong:
        Result := PInt32(aPointer)^;
      otULong:
        Result := LongInt(PUInt32(aPointer)^);

      otSQWord:
        Result := LongInt(PInt64(aPointer)^);
      otUQWord:
        Result := LongInt(PUInt64(aPointer)^);

    end;
  end;

  function GetFloatValue: Extended;
  begin
    case td^.FloatType of
      ftSingle:
        Result := PSingle(aPointer)^;
      ftDouble:
        Result := PDouble(aPointer)^;
      ftExtended:
        Result := PExtended(aPointer)^;
      ftComp:
        Result := PComp(aPointer)^;
      ftCurr:
        Result := PCurrency(aPointer)^;
    end;
  end;

var
  s, i: LongInt;
  f: Extended;
begin
  Result := Nil;
  td := GetTypeData(aTypeInfo);
  Case aTypeInfo^.Kind of
    tkUnknown :
      Error(SErrUnknownFieldKind, [aName]);
    tkInteger :
      Result := GetIntegerField;
    tkEnumeration :
      begin
        i := GetOrdValue;
        if jsoEnumeratedAsInteger in Options then
          Result := TJSONIntegerNumber.Create(i)
        else
          Result := TJSONString.Create(GetEnumName(aTypeInfo,i));
      end;
    tkFloat :
      begin
        f := GetFloatValue;
        if (aTypeInfo=TypeInfo(TDateTime)) and (jsoDateTimeAsString in Options) then
          Result := FormatDateProp(f)
        else
          Result := TJSONFloatNumber.Create(f);
      end;
    tkSet :
      if jsoSetAsString in Options then
        Result := TJSONString.Create(SetToString(aTypeInfo, GetOrdValue, jsoSetBrackets in Options))
      else begin
        s := GetOrdValue;
        Result := TJSONArray.Create;
        try
          for i:=0 to 31 do
            if i in TSet(s) then
              if jsoSetEnumeratedAsInteger in Options then
                TJSONArray(Result).Add(i)
              else
                TJSONArray(Result).Add(GetEnumName(td^.CompType, i));
        except
          FreeAndNil(Result);
          raise;
        end;
      end;
    tkChar:
      Result := TJSONString.Create(Char(GetOrdValue));
    tkSString :
      Result := TJSONString.Create(PShortString(aPointer)^);
    tkLString,
    tkAString :
      Result := TJSONString.Create(PAnsiString(aPointer)^);
    tkWString :
      Result := TJSONString.Create(PWideString(aPointer)^);
    tkVariant:
      Result := StreamVariant(PVariant(aPointer)^);
    tkClass:
      Result := StreamClassProperty(TObject(aPointer));
    tkWChar :
      Result := TJSONString.Create(WideChar(GetOrdValue));
    tkBool :
      Result := TJSONBoolean.Create(GetOrdValue<>0);
    tkInt64 :
      Result := TJSONInt64Number.Create(PInt64(aPointer)^);
    tkQWord :
      Result := TJSONFloatNumber.Create(PQWord(aPointer)^);
    tkRecord :
      Result := RecordToJSON(aPointer, aTypeInfo);
    tkArray :
      Result := StreamArray(aPointer, aTypeInfo);
    tkObject,
    tkInterface,
    tkDynArray,
    tkInterfaceRaw,
    tkProcVar,
    tkMethod :
      Error(SErrUnsupportedFieldKind, [aName]);
    tkUString :
      Result := TJSONString.Create(PUnicodeString(aPointer)^);
    tkUChar:
      Result := TJSONString.Create(UnicodeChar(GetOrdValue));
  end;
end;

function TJSONStreamerHelper.RecordToJSON(aRecord: Pointer; aTypeInfo: PTypeInfo
  ): TJSONObject;
var
  td: PTypeData;
  mf: PManagedField;
  i: LongInt;
  name: TJSONStringType;
  fd: TJSONData;
begin
  if not Assigned(aRecord) or not Assigned(aTypeInfo) then
    Exit(Nil);
  if aTypeInfo^.Kind <> tkRecord then
    Exit(Nil);
  Result := TJSONObject.Create;
  try
    td := GetTypeData(aTypeInfo);

    { ToDo: check for targets with FPC_REQUIRES_PROPER_ALIGNMENT set }
    mf := PManagedField(PByte(@td^.ManagedFldCount) + SizeOf(td^.ManagedFldCount));

   // mf := PManagedField(AlignTypeData(PByte(@td^.TotalFieldCount) + SizeOf(td^.TotalFieldCount)));

    for i := 0 to {$ifdef ver3_0}td^.ManagedFldCount{$else}td^.TotalFieldCount{$endif} - 1 do begin
      name := 'Field' + IntToStr(i + 1);
      fd := StreamField(PByte(aRecord) + mf^.FldOffset, mf^.TypeRef, name);
      if Assigned(fd) then begin
        if jsoLowerPropertyNames in Options then
          name := LowerCase(name);
        Result.Add(name, fd);
      end;
      Inc(mf);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;


generic function TJSONStreamerHelper.RecordToJSON<T>(constref aRecord: T): TJSONObject;
begin
  Result := RecordToJSON(@aRecord, TypeInfo(aRecord));
end;


function TJSONStreamerHelper.RecordToJSONString(aRecord: Pointer;
  aTypeInfo: PTypeInfo): TJSONStringType;
var
  o: TJSONData;
begin
  o := RecordToJSON(aRecord, aTypeInfo);
  try
    if jsoUseFormatString in Options then
      Result := o.FormatJSON
    else
      Result := o.AsJSON;
  finally
    o.Free;
  end;
end;


generic function TJSONStreamerHelper.RecordToJSONString<T>(constref aRecord: T): TJSONStringType;
begin
  Result := RecordToJSONString(@aRecord, TypeInfo(aRecord));
end;


end.

