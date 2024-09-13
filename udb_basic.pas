unit udb_basic;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, SQLiteUniProvider, MySQLUniProvider, Uni, DB, ujson,
  uarrays, udb_types, ustrings, usql, DataSet.Serialize, Dialogs;

type
  tnamed_conn = record
    conn: TCustomConnection;
    Name: string;
  end;

  tconns = mnarray<tnamed_conn>;

type

  { TFieldHelper }

  TFieldHelper = class helper for TField

    function set_kind(kind: TFieldKind): TField;
    function set_width(Width: integer): TField;
    function set_visible(Visible: boolean): TField;
    function set_label(lbl: string): TField;
    function set_dataset(dataset: TDataSet): TField;
    function set_field_name(Name: string): TField;
    function set_required(required: boolean): TField;
    function set_read_only(read_only: boolean): TField;
    function set_lookup(fld_lookup: tfield; dtatset_lookup: tdataset;
      lookup_key, lookup_result, key: string): tfield;
    function set_index(index: integer): tfield;
    procedure memo_filed_get_text(Sender: TField; var aText: string;
      DisplayText: boolean);
  end;

const
  default_conn_name = 'default';

procedure tconns_init(var conns: tconns);
procedure tconns_add(var conns: tconns; conn: TCustomConnection; Name: string = '');
function tconns_conn_at_index(var conns: tconns; index: integer): TCustomConnection;
procedure tconns_delete_conn_at_index(var conns: tconns; index: integer);
function tconns_conn_by_name(var conns: tconns; Name: string): TCustomConnection;
procedure tconns_free(var cons: tconns);
function connection(Name: string = ''): TCustomConnection;
procedure connection_add(conn: TCustomConnection; Name: string = '');

const
  provider_name_sqlite = 'Sqlite';
  provider_name_mysql = 'Mysql';

function connection_create(provider: string; db_name: string;
  owner: TComponent = nil; server: string = ''; port: integer = 0;
  user: string = ''; password: string = ''): TCustomConnection;
function connection_is_server(conn: TCustomConnection): boolean;
function connection_set_active(conn: TCustomConnection;
  active: boolean): TCustomConnection;
function connection_open(conn: TCustomConnection): TCustomConnection;
function connection_close(conn: TCustomConnection): TCustomConnection;
function connection_exec_sql(conn: TCustomConnection; sql: string;
  params: tvariants = nil): TCustomConnection;
function tbl_create(conn: TCustomConnection; table_name: string;
  owner: TComponent = nil): tdataset;
function qry_create_with_sql(conn: TCustomConnection; sql: string;
  params: tvariants = nil; owner: TComponent = nil): tdataset;
function qry_create(conn: TCustomConnection; parent: TComponent = nil): tdataset;
function qry_create_with_table_name(conn: TCustomConnection;
  table_name: string; owner: TComponent = nil; flds: string = '*';
  where: string = ''; params: tvariants = nil): tdataset;
function qry_set_sql(qry: tdataset; sql: string; where: string = '';
  params: tvariants = nil): tdataset;
function sql_get_fields(sql: string): string;
procedure qry_add_param(qry: tdataset; param: variant);
function qry_get_sql(qry: tdataset): string;
procedure qry_exec(qry: TDataSet);
function qry_last_inserted_id(qry: tdataset): integer;
procedure db_create_from_json(conn: TCustomConnection; json: string);
procedure db_create_from_mndatabase(conn: TCustomConnection; db: mndatabase);
procedure create_table(tbl: mntable; conn: TCustomConnection);
procedure db_update_from_json(conn: TCustomConnection; json: string);
procedure db_update_from_mndatabse(conn: TCustomConnection; db: mndatabase);
function table_exisis(table_name: string; conn: TCustomConnection): boolean;
procedure update_table(table_name: string; fields: mnfields; conn: TCustomConnection);
function field_exists(table_name, field_name: string; conn: TCustomConnection): boolean;
procedure dataset_add_fields(dataset: tdataset; db_def: mndatabase;
  tbl_name: string); overload;
procedure dataset_add_fields(dataset: tdataset; tbl: mntable); overload;
function dataset_add_field(dataset: tdataset; fld: mnfield): tfield;
function dataset_add_datetime_field(dataset: TDataSet; fld_dif: mnfield): TDateTimeField;
function dataset_add_single_field(dataset: TDataSet; fld_dif: mnfield): TFloatField;
function dataset_add_memo_string_field(dataset: TDataSet; fld_dif: mnfield): TMemoField;
function dataset_add_wide_string_field(dataset: TDataSet;
  fld_dif: mnfield): TWideStringField;
function dataset_add_bool_field(dataset: TDataSet; fld_dif: mnfield): TBooleanField;
function dataset_add_blob_field(dataset: TDataSet; fld_dif: mnfield): TBlobField;
function dataset_add_currency_field(dataset: TDataSet; fld_dif: mnfield): TCurrencyField;
function dataset_add_float_field(dataset: TDataSet; fld_dif: mnfield): TFloatField;
function field_init(fld: tfield; dataset: TDataSet; fld_dif: mnfield): tfield; overload;
function field_init(fld: tfield; dataset: TDataSet; field_name, display_label: string;
  Width, size: integer; Visible: boolean): tfield; overload;
function field_create_lookup(fld: tfield; dtatset_lookup: tdataset;
  lookup_key, lookup_result, key: string): TField;
function dataset_add_string_field(dataset: TDataSet; fld_dif: mnfield): TStringField;
  overload;
function dataset_add_string_field(dataset: TDataSet; field_name, display_label: string;
  Width, size: integer; Visible: boolean): TStringField; overload;
function dataset_add_integer_field(dataset: TDataSet; fld_dif: mnfield): TintegerField;
function dataset_add_auto_inc_field(dataset: TDataSet; fld_dif: mnfield): TAutoIncField;

function table_get_by_name(tables: mntables; Name: string): mntable;
function field_by_name(fields: mnfields; Name: string): mnfield;
procedure dataset_connect_to_master(dataset_slave: TDataSet;
  datasource_master: tdatasource; flds_master, flds_detail: string); overload;
procedure dataset_connect_to_master(dataset_slave: TDataSet;
  dataset_master: tdataset; flds_master, flds_detail: string); overload;
function get_id(table_name, where_sql: string; params: tvariants;
  conn: TCustomConnection): integer;
function get_sql_create_table(table_name: string; fields: mnfields): string;
function connection_set_db_name(connection: TCustomConnection;
  db_name: string): TCustomConnection;

var
  conns_array: tconns;

implementation


function get_sql_create_table(table_name: string; fields: mnfields): string;
var
  sql, sql_ind: string;
  i: integer;
begin
  sql := '  CREATE TABLE "' + table_name + '" (' + sLineBreak +
    '     "id" INTEGER PRIMARY KEY {if Sqlite} AUTOINCREMENT {else} AUTO_INCREMENT {endif}';
  for i := 0 to Length(fields) - 1 do
  begin
    if fields[i].is_calculated then
      Continue;

    sql := sql + ',' + sLineBreak + '     "' + fields[i].field_name +
      '" ' + fields[i].field_type;
    if fields[i].field_length > 0 then
      sql := sql + '(' + IntToStr(fields[i].field_length) + ') ';
    if fields[i].is_unique then
      sql := sql + ' UNIQUE ';
    if fields[i].is_not_null then
      sql := sql + ' NOT NULL ';
    if fields[i].default_value <> '' then
      sql := sql + ' DEFAULT ' + fields[i].default_value;
    if fields[i].is_indexed then
    begin
      if sql_ind <> '' then
      begin
        sql_ind := sql_ind + ';';
        sql_ind := sql_ind + '  CREATE INDEX "ind_' + fields[i].field_name +
          '" ON ' + table_name + '("' + fields[i].field_name + '")';
      end;
    end;
    //sql:= sql+sLineBreak;
  end;
  sql := sql + ');';
  if sql_ind <> '' then
    sql := sql + sLineBreak + '  ' + sql_ind + ';';
  Result := sql;
end;

function connection_set_db_name(connection: TCustomConnection;
  db_name: string): TCustomConnection;
begin
  TUniConnection(connection).Database := db_name;
  Result := connection;
end;




function get_id(table_name, where_sql: string; params: tvariants;
  conn: TCustomConnection): integer;
var
  qry_tmp: tuniquery;
  i: integer;
begin
  if not Assigned(conn) then
    conn := connection('default');
  qry_tmp := tuniquery.Create(nil);
  try
    qry_tmp.Connection := TUniConnection(conn);
    qry_tmp.sql.Text := 'select id from ' + table_name;
    if where_sql <> '' then
      qry_tmp.sql.Text := qry_tmp.sql.Text + ' WHERE ' + where_sql + ' LIMIT 1';
    if length(params) > 0 then
    begin
      for i := 0 to length(params) - 1 do
      begin
        qry_tmp.Params[i].Value := params[i];
      end;
    end;
    qry_tmp.Open;
    if qry_tmp.IsEmpty then
      Result := -1
    else
      Result := qry_tmp.FieldByName('id').AsInteger;
  finally
    qry_tmp.Free;
  end;
end;


function field_by_name(fields: mnfields; Name: string): mnfield;
var
  i: integer;
begin
  for i := 0 to Length(fields) - 1 do
  begin
    if fields[i].field_name = Name then
    begin
      Result := fields[i];
      Exit;
    end;
  end;
  raise Exception.Create('Cant find field' + Name);
end;

function table_get_by_name(tables: mntables; Name: string): mntable;
var
  table: mntable;
begin
  for table in tables do
    if table.table_name = Name then
    begin
      Result := table;
      exit;
    end;
  raise Exception.Create('can`t find table ' + Name);
end;

procedure dataset_add_fields(dataset: tdataset; db_def: mndatabase; tbl_name: string);
begin
  dataset_add_fields(dataset, table_get_by_name(db_def.tables, tbl_name));
end;

procedure dataset_add_fields(dataset: tdataset; tbl: mntable);
var
  fld: mnfield;
begin
  dataset_add_auto_inc_field(dataset, mnfield_id);
  for fld in tbl.fields do
    dataset_add_field(dataset, fld);
end;

function dataset_add_field(dataset: tdataset; fld: mnfield): tfield;
begin
  if fld.field_type = 'INTEGER' then
    Result := dataset_add_integer_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'TEXT' then
    Result := dataset_add_memo_string_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'REAL' then
    Result := dataset_add_single_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'VARCHAR' then
    Result := dataset_add_wide_string_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'BLOB' then
    Result := dataset_add_blob_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'BOOL' then
    Result := dataset_add_bool_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only)
  else if fld.field_type = 'DATETIME' then
    Result := dataset_add_datetime_field(dataset, fld)
      .set_required(fld.is_required).set_read_only(fld.is_read_only);
  if fld.is_calculated then
    Result.set_kind(TFieldKind.fkCalculated);
end;

function dataset_add_datetime_field(dataset: TDataSet; fld_dif: mnfield): TDateTimeField;
begin
  Result := TDateTimeField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_single_field(dataset: TDataSet; fld_dif: mnfield): TFloatField;
begin
  Result := TfloatField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_memo_string_field(dataset: TDataSet; fld_dif: mnfield): TMemoField;
begin
  Result := TMemoField.Create(dataset);
  field_init(Result, dataset, fld_dif);
  Result.OnGetText := Result.memo_filed_get_text;
end;

function dataset_add_wide_string_field(dataset: TDataSet;
  fld_dif: mnfield): TWideStringField;
begin
  Result := TWideStringField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_bool_field(dataset: TDataSet; fld_dif: mnfield): TBooleanField;
begin
  Result := TBooleanField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_blob_field(dataset: TDataSet; fld_dif: mnfield): TBlobField;
begin
  Result := TBlobField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_currency_field(dataset: TDataSet; fld_dif: mnfield): TCurrencyField;
begin
  Result := TCurrencyField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_float_field(dataset: TDataSet; fld_dif: mnfield): TFloatField;
begin
  Result := TFloatField.Create(dataset);
  field_init(Result, dataset, fld_dif);
  Result.Size := fld_dif.field_length;
end;

function field_init(fld: tfield; dataset: TDataSet; field_name, display_label: string;
  Width, size: integer; Visible: boolean): tfield;
begin
  fld.FieldName := field_name;
  if Width > 0 then
    fld.DisplayWidth := Width;
  if display_label = '' then
    display_label := field_name;
  fld.DisplayLabel := display_label;
  fld.Visible := Visible;
  fld.DataSet := dataset;
  fld.Size := size;
  Result := fld;
end;

function field_init(fld: tfield; dataset: TDataSet; fld_dif: mnfield): tfield;
begin
  Result := field_init(fld, dataset, fld_dif.field_name, fld_dif.display_label,
    fld_dif.display_width, fld_dif.field_length, fld_dif.is_visible);
end;

function field_create_lookup(fld: tfield; dtatset_lookup: tdataset;
  lookup_key, lookup_result, key: string): TField;
begin
  fld.FieldKind := fkLookup;
  fld.LookupDataSet := dtatset_lookup;
  fld.LookupKeyFields := lookup_key;
  fld.LookupResultField := lookup_result;
  fld.KeyFields := key;
  Result := fld;
end;

function dataset_add_string_field(dataset: TDataSet; fld_dif: mnfield): TStringField;
begin
  Result := dataset_add_string_field(dataset, fld_dif.field_name,
    fld_dif.display_label, fld_dif.display_width, fld_dif.field_length,
    fld_dif.is_visible);
end;

function dataset_add_string_field(dataset: TDataSet; field_name, display_label: string;
  Width, size: integer; Visible: boolean): TStringField;
begin
  Result := TStringField.Create(dataset);
  field_init(Result, dataset, field_name, display_label, Width, size, Visible);
end;

function dataset_add_integer_field(dataset: TDataSet; fld_dif: mnfield): TintegerField;
begin
  Result := TintegerField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

function dataset_add_auto_inc_field(dataset: TDataSet; fld_dif: mnfield): TAutoIncField;
begin
  Result := TAutoIncField.Create(dataset);
  field_init(Result, dataset, fld_dif);
end;

procedure dataset_connect_to_master(dataset_slave: TDataSet;
  datasource_master: tdatasource; flds_master, flds_detail: string);
begin
  TCustomUniDataSet(dataset_slave).MasterSource := datasource_master;
  TCustomUniDataSet(dataset_slave).MasterFields := flds_master;
  TCustomUniDataSet(dataset_slave).DetailFields := flds_detail;
end;

procedure dataset_connect_to_master(dataset_slave: TDataSet;
  dataset_master: tdataset; flds_master, flds_detail: string);
var
  dts: tdatasource;
begin
  dts := TDataSource.Create(dataset_slave);
  dts.DataSet := dataset_master;
  dataset_connect_to_master(dataset_slave, dts, flds_master, flds_detail);
end;

function field_exists(table_name, field_name: string; conn: TCustomConnection): boolean;
var
  qry: TUniQuery;
var
  i: integer;
begin
  qry := TUniQuery.Create(nil);
  try
    qry.Connection := TUniConnection(conn);
    qry.SQL.Text := 'select * from ' + table_name + ' limit 1;';
    qry.Open;
    for i := 0 to qry.FieldCount - 1 do
    begin
      if qry.Fields[i].FieldName = field_name then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  finally
    qry.Free;
  end;
end;



procedure update_table(table_name: string; fields: mnfields; conn: TCustomConnection);
var
  sql, sql_ind: string;
  i: integer;
begin
  for i := 0 to Length(fields) - 1 do
  begin
    if fields[i].is_calculated //or fields[i].is_lookup
    then
      Continue;
    if field_exists(table_name, fields[i].field_name, conn) then
      Continue;
    sql := '"' + fields[i].field_name + '" ' + fields[i].field_type;
    if fields[i].field_length > 0 then
      sql := sql + '(' + IntToStr(fields[i].field_length) + ') ';
    if fields[i].is_unique then
      raise Exception.Create('can`t add unique column to an exiting table ');
    if fields[i].default_value <> '' then
      sql := sql + ' DEFAULT ' + fields[i].default_value;
    if fields[i].is_not_null then
    begin
      if fields[i].default_value = '' then
        raise Exception.Create(
          'can`t add NOT NULL column without default value to an exiting table ');
      sql := sql + ' NOT NULL ';
    end;
    sql := 'ALTER TABLE "' + table_name + '" ADD {if Sqlite} COLUMN {endif} ' + sql;
    if fields[i].is_indexed then
    begin
      if sql_ind <> '' then
      begin
        sql_ind := sql_ind + ';';
      end;
      sql_ind := sql_ind + 'CREATE INDEX ind_' + fields[i].field_name +
        ' ON "' + table_name + '"("' + fields[i].field_name + '")';
    end;
    if sql_ind <> '' then
      sql := sql + ';' + sql_ind + ';';
    TUniConnection(conn).ExecSQL(sql);
    sql := '';
    sql_ind := '';
  end;
end;


function table_exisis(table_name: string; conn: TCustomConnection): boolean;
var
  names: TStringList;
begin
  names := TStringList.Create;
  try
    TUniConnection(conn).GetTableNames(names);
    Result := names.IndexOf(table_name) <> -1;
  finally
    names.Free;
  end;
end;

procedure db_update_from_mndatabse(conn: TCustomConnection; db: mndatabase);
var
  tbl: mntable;
  qry: TUniQuery;
  current_version: double;
begin
  //current_version := 0;
  //qry := TUniQuery.Create(nil);
  //try
  //  qry.Connection := TUniConnection(conn);
  //  qry.SQL.Text := 'select * from version';
  //  if table_exisis('version', conn) then
  //  begin
  //    qry.Open;
  //    current_version := qry.FieldByName('db_version').AsFloat;
  //  end;
  //
  //  if FloatToStr(current_version) = FloatToStr(DB.version) then
  //    Exit;
    for tbl in DB.tables do
    begin
      if table_exisis(tbl.table_name, conn) then
      begin
        if tbl.is_view then
        begin
          connection_exec_sql(conn, 'DROP VIEW "' + tbl.table_name + '";');
          connection_exec_sql(conn, tbl.create_sql);
        end
        else
          update_table(tbl.table_name, tbl.fields, conn);
      end
      else
      begin
        if tbl.is_view then
        begin
          connection_exec_sql(conn, tbl.create_sql);
        end
        else
          create_table(tbl, conn);

      end;
    end;
  //  qry.Open;
  //  qry.Edit;
  //  qry.FieldByName('db_version').AsFloat := DB.version;
  //  qry.post;
  //finally
  //  qry.Free;
  //end;
end;

procedure db_update_from_json(conn: TCustomConnection; json: string);
var
  db: mndatabase;
begin
  db := deserializer<mndatabase>(json);
  db_update_from_mndatabse(conn, db);
end;

procedure create_table(tbl: mntable; conn: TCustomConnection);
var
  qry: TUniQuery;
begin
  TUniConnection(conn).ExecSQL(get_sql_create_table(tbl.table_name, tbl.fields));
  if tbl.default_data <> '' then
  begin
    qry := TUniQuery.Create(nil);
    qry.Connection := TUniConnection(conn);
    try
      qry.Close;
      qry.sql.Text := 'select * from ' + tbl.table_name;
      qry.Open;
      qry.LoadFromJSON(tbl.default_data);

    finally
      qry.Free;
    end;

  end;
end;

procedure db_create_from_mndatabase(conn: TCustomConnection; db: mndatabase);
var
  tbl: mntable;
begin
  for tbl in DB.tables do
  begin
    create_table(tbl, conn);
  end;
end;

procedure db_create_from_json(conn: TCustomConnection; json: string);
var
  db: mndatabase;
begin
  db := deserializer<mndatabase>(json);
  db_create_from_mndatabase(conn, db);
end;

procedure tconns_init(var conns: tconns);
var
  named: tnamed_conn;
begin
  mnarray_init<tnamed_conn>(conns);
  named.conn := nil;
  named.Name := default_conn_name;
  mnarray_add<tnamed_conn>(conns, named);
end;

procedure tconns_add(var conns: tconns; conn: TCustomConnection; Name: string);
var
  named: tnamed_conn;
  i: integer;
begin
  if Name = '' then Name := default_conn_name;
  for i := 0 to conns.Count - 1 do
  begin
    if conns.Data[i].Name = Name then
    begin
      if assigned(conns.Data[i].conn) then
        conns.Data[i].conn.Free;
      conns.Data[i].conn := conn;
      exit;
    end;
  end;
  named.conn := conn;
  named.Name := Name;
  mnarray_add<tnamed_conn>(conns, named);

end;

function tconns_conn_at_index(var conns: tconns; index: integer): TCustomConnection;
begin
  Result := mnarray_item_at_ind<tnamed_conn>(conns, index).conn;
end;

procedure tconns_delete_conn_at_index(var conns: tconns; index: integer);
begin
  mnarray_delete_item_at_index<tnamed_conn>(conns, index);
end;

function tconns_conn_by_name(var conns: tconns; Name: string): TCustomConnection;
var
  i: integer;
begin
  for i := 0 to conns.Count - 1 do
  begin
    if conns.Data[i].Name = Name then
      Result := conns.Data[i].conn;
    exit;
  end;
  raise Exception.Create('no connection with the name:' + Name);
end;

procedure tconns_free(var cons: tconns);
var
  i: integer;
begin
  for i := 0 to cons.Count - 1 do
  begin
    if assigned(cons.Data[i].conn) then
    begin
      cons.Data[i].conn.Free;
      cons.Data[i].conn := nil;
    end;
  end;
end;

function connection(Name: string): TCustomConnection;
begin
  if Name = '' then
    Result := conns_array.Data[0].conn
  else
    Result := tconns_conn_by_name(conns_array, Name);
end;

procedure connection_add(conn: TCustomConnection; Name: string);
begin
  tconns_add(conns_array, conn, Name);
end;

function connection_create(provider: string; db_name: string;
  owner: TComponent; server: string; port: integer; user: string;
  password: string): TCustomConnection;
var
  conn: TUniConnection;
begin
  conn := TUniConnection.Create(owner);
  conn.ProviderName := provider;
  if provider = 'Sqlite' then
  begin
    conn.SpecificOptions.Add('ForceCreateDatabase=True');
    conn.SpecificOptions.Add('Direct=True');
    conn.SpecificOptions.Add('UseUnicode=True');
    conn.SpecificOptions.Add('LockingMode=lmNormal');
    conn.SpecificOptions.Add('ForceCreateDatabase=True');
    conn.Database := db_name;
  end
  else if provider = 'Mysql' then
  begin
    conn.SpecificOptions.Add('UseUnicode=True');
    conn.Database := db_name;
    conn.Server := server;
    conn.port := port;
    conn.Username := user;
    conn.Password := password;
  end;
  Result := conn;
end;

function connection_is_server(conn: TCustomConnection): boolean;
begin
  if TUniConnection(conn).ProviderName = provider_name_sqlite then
    Result := False
  else if TUniConnection(conn).ProviderName = provider_name_mysql then
    Result := True;
end;

function connection_set_active(conn: TCustomConnection;
  active: boolean): TCustomConnection;
begin
  if active then
    TUniConnection(conn).Open
  else
    TUniConnection(conn).Close();
  Result := conn;
end;

function connection_open(conn: TCustomConnection): TCustomConnection;
begin
  TUniConnection(conn).Open;
  Result := conn;
end;

function connection_close(conn: TCustomConnection): TCustomConnection;
begin
  TUniConnection(conn).Close;
  Result := conn;
end;

function connection_exec_sql(conn: TCustomConnection; sql: string;
  params: tvariants): TCustomConnection;
begin
  TUniConnection(conn).ExecSQL(sql, params);
  Result := conn;
end;

function tbl_create(conn: TCustomConnection; table_name: string;
  owner: TComponent): tdataset;
var
  tbl: TUniTable;
begin
  tbl := TUniTable.Create(owner);
  tbl.Connection := TUniConnection(conn);
  tbl.TableName := table_name;
  Result := tbl;
end;

function qry_create_with_sql(conn: TCustomConnection; sql: string;
  params: tvariants; owner: TComponent): tdataset;
var
  qry: TUniQuery;
  i: integer;
begin
  qry := tuniquery.Create(owner);
  qry.Connection := TUniConnection(conn);
  qry.SQL.Text := sql;
  for i := 0 to length(params) - 1 do
  begin
    qry.Params[i].Value := params[i];
  end;
  Result := qry;
end;

function qry_create(conn: TCustomConnection; parent: TComponent): tdataset;
begin
  Result := TUniQuery.Create(parent);
  TUniQuery(Result).Connection := TUniConnection(conn);
end;

function qry_create_with_table_name(conn: TCustomConnection;
  table_name: string; owner: TComponent; flds: string; where: string;
  params: tvariants): tdataset;
var
  sql: string;
begin
  sql := 'SELECT ' + flds + ' FROM ' + table_name;
  if where <> '' then
    sql := sql + ' WHERE ' + where;
  Result := qry_create_with_sql(conn, sql, params, owner);
end;

function qry_set_sql(qry: tdataset; sql: string; where: string;
  params: tvariants): tdataset;
var
  v: variant;
begin
  if where <> '' then
  begin
    sql := sql + ' WHERE ' + where;
    for v in params do
      TUniParam(TUniQuery(qry).Params.Add).Value := v;
  end;
  TUniQuery(qry).sql.Text := sql;
  Result := qry;
end;

function sql_get_fields(sql: string): string;
var
  strs: TStringArray;
  flds: string;
begin
  sql := LowerCase(remove_extra_spaces_for_sql_text(sql));
  sql := sql.remove(0, 7);
  strs := sql.Split(['limit']);
  Result := strs[0];
  sql := strs[1];
  flds := flds.Split(['from'])[0];

end;

procedure qry_add_param(qry: tdataset; param: variant);
var
  i: integer;
begin

  TUniParam(TUniQuery(qry).Params.Add).Value := param;
end;

function qry_get_sql(qry: tdataset): string;
begin
  if qry is TUniQuery then
    Result := TUniQuery(qry).SQL.Text
  else if qry is TUniTable then
    Result := 'select * from ' + TUniTable(qry).TableName
  else
    raise Exception.Create('unknow dataset kind');
end;

procedure qry_exec(qry: TDataSet);
begin
  TUniQuery(qry).ExecSQL;

end;

function qry_last_inserted_id(qry: tdataset): integer;
begin
  Result := TUniQuery(qry).LastInsertId;

end;


{ TFieldHelper }

function TFieldHelper.set_field_name(Name: string): TField;
begin
  self.FieldName := Name;
  Result := self;
end;

function TFieldHelper.set_kind(kind: TFieldKind): TField;
begin
  self.FieldKind := kind;
  Result := Self;
end;

function TFieldHelper.set_width(Width: integer): TField;
begin
  self.DisplayWidth := Width;
  Result := Self;
end;

function TFieldHelper.set_label(lbl: string): TField;
begin
  self.DisplayLabel := lbl;
  Result := self;
end;

function TFieldHelper.set_read_only(read_only: boolean): TField;
begin
  self.ReadOnly := read_only;
  Result := self;
end;

function TFieldHelper.set_lookup(fld_lookup: tfield; dtatset_lookup: tdataset;
  lookup_key, lookup_result, key: string): tfield;
begin
  field_create_lookup(self, dtatset_lookup, lookup_key, lookup_result, key);
  Result := self;
end;

function TFieldHelper.set_index(index: integer): tfield;
begin
  self.Index := index;
end;

procedure TFieldHelper.memo_filed_get_text(Sender: TField; var aText: string;
  DisplayText: boolean);
begin
  aText := Sender.AsString;
end;

function TFieldHelper.set_required(required: boolean): TField;
begin
  self.Required := required;
  Result := self;
end;

function TFieldHelper.set_visible(Visible: boolean): TField;
begin
  self.Visible := Visible;
  Result := self;
end;

function TFieldHelper.set_dataset(dataset: TDataSet): TField;
begin
  self.DataSet := dataset;
  Result := Self;
end;



initialization
  tconns_init(conns_array);

finalization;
  //  tconns_free(conns_array);
end.
