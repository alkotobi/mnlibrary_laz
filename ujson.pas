unit ujson;

{$mode Delphi}{$H+}

interface

uses
  {$IF DEFINED(FPC)}
    fpjson,
  {$ELSE}
  System.JSON,
  {$ENDIF}      Classes, SysUtils, DataSet.Serialize, DB, SynCommons;

function serialize<T>(obj: T): string;
function deserializer<T>(json: string): T;
function dataset_save_json(dataset: tdataset): string;
function dataset_save_json_array(dataset: tdataset): string;
procedure dataset_load_json(dataset:tdataset;json: string);




implementation


function serialize<T>(obj: T): string;
begin
  Result := RecordSaveJSON(obj, TypeInfo(obj));
end;

function deserializer<T>(json: string): T;
begin
  RecordLoadJSON(Result, json, TypeInfo(Result));
end;

function dataset_save_json(dataset: tdataset): string;
begin
  Result := dataset.ToJSONArray().ToString;
end;

function dataset_save_json_array(dataset: tdataset): string;
var
  jarray: TJSONArray;
begin
  jarray := dataset.ToJSONArray();
  try
    {$IF DEFINED(FPC)}
    result :=jarray.FormatJSON()
    {$ELSE}
    Result := jarray.format;
    {$ENDIF}
  finally
    jarray.Free;
  end;
end;

procedure dataset_load_json(dataset:tdataset;json: string);
begin
  dataset.LoadFromJSON(json);
end;

end.
