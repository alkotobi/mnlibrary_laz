unit umain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  StdCtrls, CustomDrawnControls, BCComboBox, VirtualTable, Uni, uarrays,
  ustrings, udb_types, SynCommons, // MORMOT common units
  mORMot,SynTable,udb_tools_design,ugui;

type

   { mm }

   mm=record
    a:string;
    b:double;
  end;
  { TForm1 }

  TForm1 = class(TForm)
    act: TActionList;
    act_json: TAction;
    act_strings: TAction;
    act_mnarray: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CDComboBox1: TCDComboBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    mem_log: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    UniConnection1: TUniConnection;
    VirtualTable1: TVirtualTable;
    procedure act_mnarrayExecute(Sender: TObject);
    procedure act_stringsExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  combo:mncombobox;
 const
  __TSQLRestCacheEntryValue = 'a: RawUTF8; b: double';
//-----------------



implementation

{$R *.lfm}






// Function to serialize a record to JSON
function SerializeRecord(const ARecord: mm): RawUTF8;
var
  Json: TJSONWriter;
begin
  Json := TJSONWriter.Create;
  try
    Json.Add( 'a', [ARecord.a]);
    Json.Add('b', [ARecord.b]);
    Result := Json.Text; // Get JSON content as RawUTF8
  finally
    Json.Free;
  end;
end;

{ mm }





{ TForm1 }

procedure TForm1.act_mnarrayExecute(Sender: TObject);
var
  arr: mnarray<string>;
  i: integer;
  str: string;
  named:mnamed<integer>;
  named_arr:mnarray<mnamed<integer>>;
begin
  mnarray_init<string>(arr);
  for i := 0 to 10 do
  begin
    mnarray_add<string>(arr, IntToStr(i));

    mem_log.Lines.add(arr.Data[i]);

  end;

  mem_log.Lines.add('----------------------');

  mnarray_delete_item_at_index<string>(arr,3);
   for i := 0 to arr.count-1 do
  begin
    mem_log.Lines.add(arr.Data[i]);
  end;

   mem_log.Lines.add('----------------------');

  mnarray_delete_item_at_index<string>(arr,arr.count-1 );
   for i := 0 to arr.count-1 do
  begin
    mem_log.Lines.add(arr.Data[i]);

  end;

   named.item1:='ana';
   named.item2:=5;


end;

procedure TForm1.act_stringsExecute(Sender: TObject);
 var str : string;
   var chr:char;
     sql:mnsql;
begin
   str := '     SELECT         name , type , fright  from table  where age =5 order   by   name   ,age group by age limit 5 offset 7;   ';
    chr:= str[1];
   mem_log.Lines.add(inttostr(ord(chr)));
   chr:=str[2];
      mem_log.Lines.add(inttostr(ord(chr)));
   mem_log.Lines.add( remove_extra_spaces_for_sql_text(str));
   sql.init_from_text(str)
end;

procedure TForm1.Button1Click(Sender: TObject);


 var m1,m2:mm;
   utf:RawUTF8;
   db:mndatabase;
   tbl:mntable;
   fld:mnfield;
   str:string;
begin

  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(mm),__TSQLRestCacheEntryValue);

  m1.a:='fdffd';
  m1.b:=1.234;
  utf:=RecordSaveJSON(m1,TypeInfo(m1));
  memo1.Text:=utf;
   TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(fld),__mnfield_entry_value).Options := [soReadIgnoreUnknownFields,soWriteHumanReadable];
     TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(tbl),__mntable_entry_value).Options := [soReadIgnoreUnknownFields,soWriteHumanReadable];

       TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(db),__mndatabase_entry_value).Options := [soReadIgnoreUnknownFields,soWriteHumanReadable];


end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  combo.init(ComboBox1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  combo.is_search_with_contains:=not combo.is_search_with_contains;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
   edit1.text:=get_clean_text(edit1.Text);

end;

end.
