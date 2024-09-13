unit udataset;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB, VirtualTable, uarrays, udb_types, udb_basic;

type
  mndataset_notify_events = mnarray<TDataSetNotifyEvent>;

  { mndataset }

  mndataset = record
  private
    fdataset: tdataset;
    function get_dataset: tdataset;
    procedure virtual_table_before_post(dataset: tdataset);
    procedure before_post(dataset: tdataset);
  public
    sql: mnsql;
    params: tvariants;
    owner: TComponent;
    connection: TCustomConnection;
    online_mode: boolean;
    evnts_before_edit: mndataset_notify_events;
    evnts_after_edit: mndataset_notify_events;
    evnts_before_post: mndataset_notify_events;
    evnts_after_post: mndataset_notify_events;
    evnts_before_delete: mndataset_notify_events;
    evnts_after_delete: mndataset_notify_events;
    evnts_before_insert: mndataset_notify_events;
    evnts_after_insert: mndataset_notify_events;
    property dataset: tdataset read get_dataset;
    constructor Create(sql: string; params: tvariants; online_mode: boolean;
      conn: TCustomConnection; owner: TComponent);
    procedure Destroy();
    procedure add_event(var events:mndataset_notify_events;event:TDataSetNotifyEvent);
  end;



implementation

{ mndataset }

function mndataset.get_dataset: tdataset;
begin
  Result := fdataset;
end;

constructor mndataset.Create(sql: string; params: tvariants;
  online_mode: boolean; conn: TCustomConnection; owner: TComponent);
var
  i: integer;
  qry:TDataset;
begin
  self.sql.init_from_text(sql);
  self.online_mode := online_mode;
  if Assigned(conn) then
    self.connection := conn
  else
    self.connection := conns_array.Data[0].conn;
  for i := 0 to length(params) - 1 do
    self.params[i] := params[i];
  self.owner := owner;
  if online_mode then
  begin
    fdataset := qry_create_with_sql(self.connection, sql, params, owner);
  end
  else
  begin
    fdataset := TVirtualTable.Create(owner);
    fdataset.BeforePost := before_post;
    self.add_event(self.evnts_before_post,virtual_table_before_post);
    qry := qry_create_with_sql(connection,sql,params,nil);
    try
      qry.Open;
      TVirtualTable(fdataset).Assign(qry);
    finally
      qry.Free;
    end;
  end;

end;

procedure mndataset.Destroy();
begin
  self.dataset.Free;
end;

procedure mndataset.add_event(var events: mndataset_notify_events;
  event: TDataSetNotifyEvent);
begin
  mnarray_add<TDataSetNotifyEvent>(events,event);
end;

procedure mndataset.virtual_table_before_post(dataset: tdataset);
var
  qry: tdataset;
  sql: string;
  i: integer;
begin
  if dataset.State = dsInsert then
  begin
    sql := self.sql.sql_insert_paramerable();
    qry := qry_create_with_sql(self.connection, sql, nil);
    try
      for i := 0 to length(self.sql.insert_fields) - 1 do
      begin
        qry_add_param(qry, dataset.FieldByName(self.sql.insert_fields[i]).Value);
      end;
      qry_exec(qry);
      dataset.FieldByName('id').AsInteger := qry_last_inserted_id(qry);
    finally
      qry.Free;
    end;

  end
  else
  begin
    sql := string.Join(',', self.sql.insert_fields);
    qry := qry_create_with_table_name(self.connection, self.sql.table_name,
      nil, sql, 'id=:id', [dataset.FieldByName('id').Value]);
    try
      qry.Open;
      qry.edit;
      for i:=0 to length(self.sql.insert_fields)-1 do
      begin
        qry.FieldByName(self.sql.insert_fields[i]).Value:=dataset.FieldByName(self.sql.insert_fields[i]).Value;
      end;
      qry.Post;

    finally
      qry.Free;
    end;
  end;

end;

procedure mndataset.before_post(dataset: tdataset);
var
  evnt: TDataSetNotifyEvent;
begin
  for evnt in self.evnts_before_post.Data do
    evnt(dataset);

end;



end.
