unit usql;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,udb_types;

 function get_sql_create_table(table_name: string; fields: mnfields): string;


implementation
 
function get_sql_create_table(table_name: string; fields: mnfields): string;
var
  sql, sql_ind: string;
  i: Integer;
begin
  sql := '  CREATE TABLE "' + table_name + '" (' + sLineBreak +
    '     "id" INTEGER PRIMARY KEY {if Sqlite} AUTOINCREMENT {else} AUTO_INCREMENT {endif}';
  for i := 0 to Length(fields) - 1 do
  begin
    if fields[i].is_calculated then
      Continue;

    sql := sql + ',' + sLineBreak + '     "' + fields[i].field_name + '" ' +
      fields[i].field_type;
    if fields[i].field_length > 0 then
      sql := sql + '(' + inttostr(fields[i].field_length) + ') ';
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
        sql_ind := sql_ind + '  CREATE INDEX "ind_' + fields[i].field_name + '" ON ' +
          table_name + '("' + fields[i].field_name + '")';
      end;
    end;
    //sql:= sql+sLineBreak;
  end;
  sql := sql + ');';
  if sql_ind <> '' then
    sql := sql + sLineBreak + '  ' + sql_ind + ';';
  Result := sql;
end;

end.

