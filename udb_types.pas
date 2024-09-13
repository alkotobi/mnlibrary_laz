unit udb_types;
{$mode delphi}{$H+}
interface

uses Classes, SysUtils, ustrings, SynCommons,
  mORMot, SynTable, DB;

type
  tvariants = array of variant;
  { mnsql }

  mnsql = record
  private
    ftext: string;
    ftable_name: string;
    ffields: string;
    fwhere: string;
    forderby: string;
    fgrouped_by: string;
    flimit: integer;
    foffset: integer;
    fparams: tvariants;
    fchanged: boolean;
    finsert_fields: TStringArray;
    function get_changed: boolean;
    function get_insert_fields: TStringArray;
    function get_text: string;
    procedure Setfields(AValue: string);
    procedure Setgrouped_by(AValue: string);
    procedure Setinsert_fields(AValue: TStringArray);
    procedure Setlimit(AValue: integer);
    procedure Setoffset(AValue: integer);
    procedure Setorderby(AValue: string);
    procedure Setparams(AValue: tvariants);
    procedure Settable_name(AValue: string);
    procedure Setwhere(AValue: string);
    procedure set_changed(AValue: boolean);
  public
    property insert_fields: TStringArray read get_insert_fields write Setinsert_fields;
    property table_name: string read Ftable_name write Settable_name;
    property fields: string read Ffields write Setfields;
    property where: string read Fwhere write Setwhere;
    property orderby: string read Forderby write Setorderby;
    property grouped_by: string read Fgrouped_by write Setgrouped_by;
    property limit: integer read Flimit write Setlimit;
    property offset: integer read Foffset write Setoffset;
    property params: tvariants read Fparams write Setparams;
    property Text: string read get_text;
    property changed: boolean read get_changed write set_changed;
    procedure init(table_name: string; fields: string = '*';
      where: string = ''; orderby: string = ''; grouped_by: string = '';
      limit: integer = 0; offset: integer = -1);
    procedure init_from_text(sql: string);
    function sql_insert_paramerable(): string;
  end;



  { mnfield }

  mnfield = packed record
    field_name: string;
    field_type: string;
    field_length: integer;
    is_unique: boolean;
    is_not_null: boolean;
    is_indexed: boolean;
    default_value: string;
    description: string;
    is_required: boolean;
    is_visible: boolean;
    is_read_only: boolean;
    display_width: integer;
    display_label: string;
    is_calculated: boolean;
    ind:integer;
    //is_lookup:boolean;
    function init_from_db(dataset: tdataset): mnfield;
  end;

const
  mnfield_id: mnfield = (field_name: 'id';
    field_type: 'INTEGER';
    field_length: 0;
    is_unique: True;
    is_not_null: True;
    is_indexed: False;
    default_value: '';
    description: '';
    is_required: True;
    is_visible: False;
    is_read_only: False;
    display_width: 20;
    display_label: '';
    is_calculated: False;
    ind:0;
    //is_lookup:false
    );
  __mnfield_entry_value =
    'field_name: string;field_type: string;field_length: integer;' +
    'is_unique: boolean;is_not_null: boolean;' +
    'is_indexed: boolean;default_value: string;' +
    'description: string;is_required: boolean;' +
    'is_visible: boolean;is_read_only: boolean;' +
    'display_width: integer;display_label: string;is_calculated: boolean;ind:integer;';

type
  mnfields = array of mnfield;

  { mntable }

  mntable = packed record
    table_name: string;
    fields: mnfields;
    default_data: string;
    description: string;
    insert_sql: string;
    insert_params_count: integer;
    is_view:boolean;
    create_sql:string;
    function add_field(fld: mnfield): mntable;
    procedure init;
    function init_from_db(tbl_tables, tbl_fields: tdataset): mntable;
    function create_insert_sql(table_name: string; fields: mnfields): string;
  end;

const
  __mntable_entry_value = 'table_name: string;' + 'fields: [' + __mnfield_entry_value
    + '];' + 'default_data: string;' + 'description: string;' +
    'insert_sql: string;' + 'insert_params_count: integer;is_view:boolean;create_sql:string; ';

type
  mntables = array of mntable;

  { mndatabase }

  mndatabase = packed record
    database_name: string;
    tables: mntables;
    version: double;
    description: string;
    db_path:string;
    procedure init;
  end;

const
  __mndatabase_entry_value = '    database_name: string;' + 'tables: ['+__mntable_entry_value+ '];' +
    'version: double;' + 'description: string;db_path:string;';



implementation



{ mnsql }

function mnsql.get_text: string;
begin
  if not changed then
  begin
    Result := ftext;
    exit;
  end;
  Result := 'SELECT ' + fields + ' FROM ' + table_name;
  if where <> '' then
    Result := Result + ' WHERE ' + where;
  if orderby <> '' then
  begin
    Result := Result + 'ORDER BY ' + orderby;
  end;
  if grouped_by <> '' then
    Result := Result + ' GROUP BY ' + grouped_by;
  if limit > 0 then
  begin
    Result := Result + ' limit ' + IntToStr(limit);
    if offset >= 0 then
      Result := Result + ' OFFSET ' + IntToStr(offset);
  end;
  Result := Result + ';';
  ftext := Result;
  changed := False;
end;

function mnsql.get_changed: boolean;
begin
  Result := fchanged;
end;

function mnsql.get_insert_fields: TStringArray;
var
  strs, strs2: TStringArray;
  table_name, str: string;
  flds: TStringArray;
begin
  if finsert_fields <> nil then
  begin
    Result := finsert_fields;
    exit;
  end;
  strs := Self.table_name.Split(',');
  table_name := strs[0];
  strs := self.fields.Split(',');
  for str in strs do
  begin
    strs2 := str.Split('.');
    if Length(strs2) = 2 then
    begin
      if strs2[0] = table_name then
      begin
        SetLength(flds, length(flds) + 1);
        flds[length(flds) - 1] := strs2[1];
      end;
    end
    else
    begin
      SetLength(flds, length(flds) + 1);
      flds[length(flds) - 1] := strs2[0];
    end;
  end;
  finsert_fields := flds;
  Result := flds;
end;

procedure mnsql.Setfields(AValue: string);
begin
  if Ffields = AValue then Exit;
  Ffields := AValue;
  changed := True;
end;

procedure mnsql.Setgrouped_by(AValue: string);
begin
  if Fgrouped_by = AValue then Exit;
  Fgrouped_by := AValue;
  changed := True;
end;

procedure mnsql.Setinsert_fields(AValue: TStringArray);
begin
  if Finsert_fields = AValue then Exit;
  Finsert_fields := AValue;
end;

procedure mnsql.Setlimit(AValue: integer);
begin
  if Flimit = AValue then Exit;
  Flimit := AValue;
  changed := True;
end;

procedure mnsql.Setoffset(AValue: integer);
begin
  if Foffset = AValue then Exit;
  Foffset := AValue;
  changed := True;
end;

procedure mnsql.Setorderby(AValue: string);
begin
  if Forderby = AValue then Exit;
  Forderby := AValue;
  changed := True;
end;

procedure mnsql.Setparams(AValue: tvariants);
begin
  if Fparams = AValue then Exit;
  Fparams := AValue;
  changed := True;
end;

procedure mnsql.Settable_name(AValue: string);
begin
  if Ftable_name = AValue then Exit;
  Ftable_name := AValue;
  changed := True;
end;

procedure mnsql.Setwhere(AValue: string);
begin
  if Fwhere = AValue then Exit;
  Fwhere := AValue;
  changed := True;
end;

procedure mnsql.set_changed(AValue: boolean);
begin
  fchanged := AValue;
  if fchanged then  SetLength(finsert_fields, 0);
end;

procedure mnsql.init(table_name: string; fields: string; where: string;
  orderby: string; grouped_by: string; limit: integer; offset: integer);
begin
  self.table_name := table_name;
  self.fields := fields;
  self.where := where;
  self.orderby := orderby;
  self.grouped_by := grouped_by;
  self.limit := limit;
  self.offset := offset;
  changed := True;
end;

procedure mnsql.init_from_text(sql: string);
var
  strs: TStringArray;
  flds: string;
begin
  ftext := sql;
  changed := False;
  sql := LowerCase(remove_extra_spaces_for_sql_text(sql));
  sql := sql.remove(0, 7).Replace(';', #0);

  strs := sql.Split(['offset']);
  if Length(strs) = 2 then
    self.offset := StrToInt(remove_space(strs[1]))
  else
    self.offset := -1;
  sql := strs[0];
  strs := sql.Split(['limit']);
  if Length(strs) = 2 then
    self.limit := StrToInt(remove_space(strs[1]))
  else
    self.limit := 0;

  sql := strs[0];
  strs := sql.Split(['group by']);
  if Length(strs) = 2 then
    self.grouped_by := remove_space(strs[1])
  else
    self.grouped_by := '';

  sql := strs[0];
  strs := sql.Split(['order by']);
  if Length(strs) = 2 then
    self.orderby := remove_space(strs[1])
  else
    self.orderby := '';

  sql := strs[0];
  strs := sql.Split(['where']);
  if Length(strs) = 2 then
    self.where := remove_space(strs[1])
  else
    self.where := '';

  sql := strs[0];
  strs := sql.Split(['from']);
  if Length(strs) = 2 then
    self.table_name := remove_space(strs[1])
  else
    raise Exception.Create('wrong sql:' + sql);

  self.fields := remove_space(strs[0]);
end;

function mnsql.sql_insert_paramerable(): string;
var
  flds, strs: TStringArray;
  i: integer;
  params: string;
  table_name: string;
begin
  strs := table_name.Split(',');
  table_name := strs[0];
  flds := self.fields.Split(',');
  params := ':' + flds[0];
  for i := 1 to Length(flds) - 1 do
  begin
    params := params + ',:' + flds[i];
  end;
  Result := 'INSERT INTO ' + self.table_name + '(' + self.fields +
    ') values(' + params + ');';
end;

{ mnfield }

function mnfield.init_from_db(dataset: tdataset): mnfield;
begin
  self.field_name := dataset.FieldByName('field_name').AsString;
  self.field_type := dataset.FieldByName('field_type').AsString;
  self.field_length := dataset.FieldByName('field_length').AsInteger;
  self.is_unique := dataset.FieldByName('is_unique').AsBoolean;
  self.is_not_null := dataset.FieldByName('is_not_null').AsBoolean;
  self.is_indexed := dataset.FieldByName('is_indexed').AsBoolean;
  self.default_value := dataset.FieldByName('default_value').AsString;
  self.description := dataset.FieldByName('description').AsString;
  self.is_required := dataset.FieldByName('is_required').AsBoolean;
  self.is_visible := dataset.FieldByName('is_visible').AsBoolean;
  self.is_read_only := dataset.FieldByName('is_read_only').AsBoolean;
  self.display_width := dataset.FieldByName('display_width').AsInteger;
  self.display_label := dataset.FieldByName('display_label').AsString;
  self.is_calculated := dataset.FieldByName('is_calculated').AsBoolean;
  self.ind:=dataset.FieldByName('ind').AsInteger;
  Result := self;
end;

{ mntable }

function mntable.add_field(fld: mnfield): mntable;
begin
  SetLength(self.fields, Length(self.fields) + 1);
  self.fields[Length(self.fields) - 1] := fld;
  Result := self;
end;

procedure mntable.init;
begin
  setlength(self.fields,0);
  self.default_data:='';
  self.description:='';
  self.insert_params_count:=0;
  self.insert_sql:='';
  self.table_name:=''
end;

function mntable.create_insert_sql(table_name: string; fields: mnfields): string;
var
  flds, params: string;
  i: integer;
begin
  Result := '    INSERT INTO "' + table_name + '"(';
  flds := '"' + fields[0].field_name + '"';
  params := ':' + fields[0].field_name;
  for i := 1 to Length(fields) - 1 do
  begin
    if fields[i].is_calculated //or fields[i].is_lookup
    then
      Continue;
    flds := flds + ',"' + fields[i].field_name + '"';
    params := params + ',:' + fields[i].field_name;
  end;
  Result := Result + flds + ')' + sLineBreak + '    VALUES(' + params + ');';
  self.insert_sql := Result;
end;

function mntable.init_from_db(tbl_tables, tbl_fields: tdataset): mntable;
var
  fld: mnfield;
begin
  self.init;
  if tbl_fields.IsEmpty then
    raise Exception.Create('empty fields');
  if tbl_tables.IsEmpty then
    raise Exception.Create('empty tables');
  self.table_name := tbl_tables.FieldByName('table_name').AsString;
  self.default_data := tbl_tables.FieldByName('default_data').AsString;
  self.description := tbl_tables.FieldByName('description').AsString;
  self.is_view:=tbl_tables.FieldByName('is_view').AsBoolean;
  self.create_sql:=tbl_tables.FieldByName('create_sql').AsString;
  tbl_fields.First;
  while not tbl_fields.EOF do
  begin
    fld.init_from_db(tbl_fields);
    self.add_field(fld);
    tbl_fields.Next;
  end;
  self.insert_params_count := Length(self.fields);
  self.create_insert_sql(self.table_name, self.fields);
  Result := self;
end;

{ mndatabase }

procedure mndatabase.init;
begin
  self.description:='';
  self.database_name:='';
  setlength(self.tables,0);
  self.version:=-1;
  self.db_path:='';
end;

end.
