unit ugui;

{$mode delphi}{$H+}

interface

uses
  Classes, Controls, MaskEdit, StdCtrls, SysUtils, Dialogs, StrUtils,
  DBCtrls, uarrays, DB, Forms, Graphics, Character, Buttons;

type

  { mneditor }
  ton_validate_notify = procedure(Sender: TObject; dispay_text: string);

  mneditor = record

  private
    fold_value: string;
    function get_is_changed: boolean;
    function get_text: string;
    procedure set_text(AValue: string);
    procedure _on_exit(Sender: TObject);
    procedure _on_enter(Sender: TObject);
    procedure _on_key_pressed(Sender: TObject; var Key: char);
  public
    control: TWinControl;
    on_validate: procedure(Sender: TObject; dispay_text: string);
    procedure init(control: TWinControl; on_validate: ton_validate_notify);
    property Text: string read get_text write set_text;
    property is_changed: boolean read get_is_changed;

  end;

  { mncombobox }
  tvalidate_notify = procedure(Sender: TObject; Text: string) of object;
  mncombobox = record
  private
    Ffixed_list: boolean;
    Fon_validate: tvalidate_notify;
    fstring_array: mnstring_array;
    fint_array: mnarray<integer>;//list of original indexes
    fint_array_filtered: mnarray<integer>; //list of filtered indexes
    fon_key_up_cache: TKeyEvent;
    fon_exit_cache: TNotifyEvent;
    fis_search_with_contains: boolean;
    fdo_filter: boolean;
    function get_is_search_with_contains: boolean;
    procedure Setfixed_list(AValue: boolean);
    procedure Seton_validate(AValue: tvalidate_notify);
    procedure set_is_search_with_contains(AValue: boolean);
    procedure on_key_up_(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure on_exit_(Sender: TObject);
  public
    combo: TCustomComboBox;
    dataset: tdataset;
    list_field: string;
    id_field: string;
    procedure init(combobox: TCustomComboBox;
      search_with_contains: boolean = False);
    procedure init_from_dataset(combobox: tcombobox; dataset: tdataset;
      list_field: string; id_field: string = 'id';
      search_with_contains: boolean = False);
    procedure filter;
    procedure refresh;
    function get_current_index(): integer;
    property is_search_with_contains: boolean
      read get_is_search_with_contains write set_is_search_with_contains;
    property fixed_list: boolean read Ffixed_list write Setfixed_list;
    property on_validate: tvalidate_notify read Fon_validate write Seton_validate;


  end;

  { mncustum_control }

  mncustum_control = record


    fon_paint_cach: TNotifyEvent;

    control: TControl;
    color_brash: tcolor;
    color_font: tcolor;
    color_pen: tcolor;
    alignemt_text: TAlignment;
    alignemt_bitmap: TAlignment;
    padding_bitmap: integer;
    bitmap: tbitmap;
    image_list:TImageList;
    image_index:integer;
    on_paint_custom: TNotifyEvent;
    is_rounded: boolean;
    procedure on_paint(Sender: TObject);
    procedure init(control: TControl; bitmap: tbitmap=nil;
      on_paint_custom: TNotifyEvent = nil; color_brash: tcolor = clgreen;
      color_font: tcolor = clred; color_pen: tcolor = clYellow; is_rounded: boolean = True);
    procedure finalize;
  end;

function color_get_pascal_hex_from_web_hex(web_hex: string): string;
function dlg_confirmed(Value: string): boolean;



implementation

{mncombobox}
function mncombobox.get_is_search_with_contains: boolean;
begin
  Result := fis_search_with_contains;
end;

procedure mncombobox.Setfixed_list(AValue: boolean);
begin
  if Ffixed_list = AValue then Exit;
  Ffixed_list := AValue;
end;


procedure mncombobox.Seton_validate(AValue: tvalidate_notify);
begin
  Fon_validate := AValue;
end;

procedure mncombobox.set_is_search_with_contains(AValue: boolean);
begin
  fis_search_with_contains := AValue;
end;

procedure mncombobox.on_key_up_(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Assigned(fon_key_up_cache) then
    fon_key_up_cache(Sender, key, shift);
  if key = 8 then fdo_filter := False
  else
    fdo_filter := True;
  if key = 13 then
    if Assigned(on_validate) then on_validate(Sender, combo.Text);
  filter;
end;

procedure mncombobox.on_exit_(Sender: TObject);
begin
  if Assigned(fon_exit_cache) then
    fon_exit_cache(Sender);
  if Ffixed_list then
  begin
    if (combo.Items.Count > 0) and (combo.ItemIndex < 0) then
      combo.ItemIndex := 0
    else if combo.Items.Count = 0 then
    begin
      ShowMessage(combo.Text + ' not in the list');
      abort;
    end;
  end;
  if Assigned(on_validate) then on_validate(Sender, combo.Text);
end;

function mncombobox.get_current_index(): integer;
begin
  if combo.ItemIndex >= 0 then
    Result := fint_array_filtered.Data[combo.ItemIndex]
  else
    Result := -1;
end;

procedure mncombobox.init(combobox: TCustomComboBox; search_with_contains: boolean);
var
  i: integer;
begin
  dataset := nil;
  list_field := '';
  id_field := '';
  fon_exit_cache := nil;
  fon_key_up_cache := nil;
  fdo_filter := True;
  Ffixed_list := False;
  mnstring_array_init_from_tstrings(combobox.Items, fstring_array);
  fint_array.init(combobox.items.Count);
  fint_array_filtered.init_from_mnarray(fint_array);
  for i := 0 to combobox.items.Count - 1 do
  begin
    fint_array.Data[i] := i;
  end;
  fint_array.Count := combobox.items.Count;
  combo := combobox;
  combo.OnExit := on_exit_;
  if combo is TComboBox then
  begin
    if Assigned(TComboBox(combo).OnKeyUp) then
      fon_key_up_cache := TComboBox(combo).OnKeyUp;
    TComboBox(combo).OnKeyUp := self.on_key_up_;
  end
  else if combo is TDBComboBox then
  begin
    if Assigned(TDBComboBox(combo).OnChange) then
      fon_key_up_cache := TDBComboBox(combo).OnKeyUp;
    TDBComboBox(combo).OnKeyUp := self.on_key_up_;
  end
  else if combo is TDBLookupComboBox then
  begin
    if Assigned(TDBLookupComboBox(combo).OnChange) then
      fon_key_up_cache := TDBLookupComboBox(combo).OnKeyUp;
    TDBLookupComboBox(combo).OnKeyUp := self.on_key_up_;
  end;
  //combo.Items.AddStrings(fstring_array.Data);
  self.is_search_with_contains := search_with_contains;
  //combo.AutoDropDown := True;
end;

procedure mncombobox.init_from_dataset(combobox: tcombobox; dataset: tdataset;
  list_field: string; id_field: string; search_with_contains: boolean);
var
  i: integer;
  bk: TBookMark;
begin
  self.init(combobox, search_with_contains);
  self.dataset := dataset;
  self.id_field := id_field;
  self.list_field := list_field;
  bk := dataset.GetBookmark;
  dataset.DisableControls;
  dataset.First;
  fstring_array.init(dataset.RecordCount);
  fint_array.init(dataset.RecordCount);
  i := 0;
  while not dataset.EOF do
  begin
    fstring_array.Data[i] := dataset.FieldByName(list_field).AsString;
    fint_array.Data[i] := dataset.FieldByName(id_field).AsInteger;
    i := i + 1;
    dataset.Next;
  end;
  fstring_array.Count := dataset.RecordCount;
  fint_array.Count := dataset.RecordCount;
  fint_array_filtered.init_from_mnarray(fint_array);
  combo.Items.Clear;
  combo.Items.AddStrings(fstring_array.Data);
  dataset.GotoBookmark(bk);
  dataset.EnableControls;
end;

procedure mncombobox.filter;
var
  i: integer;
  start_sel, end_sel: integer;
begin
  if self.combo.Text = '' then
  begin
    self.combo.Items.Clear;
    self.combo.Items.AddStrings(fstring_array.Data);
    fint_array_filtered.init_from_mnarray(fint_array);
  end
  else
  begin
    self.combo.Items.Clear;
    fint_array_filtered.Clear;
    for i := 0 to self.fstring_array.Count - 1 do
    begin
      if (not is_search_with_contains) and
        (self.fstring_array.Data[i].StartsWith(self.combo.Text)) then
      begin
        self.combo.Items.Add(self.fstring_array.Data[i]);
        self.fint_array_filtered.add(fint_array.Data[i]);
      end
      else if (is_search_with_contains) and
        (self.fstring_array.Data[i].Contains(self.combo.Text)) then
      begin
        self.combo.Items.Add(self.fstring_array.Data[i]);
        self.fint_array_filtered.add(fint_array.Data[i]);
      end;
    end;
    if not is_search_with_contains and fdo_filter then
    begin
      if self.combo.Items.Count > 0 then
      begin
        fdo_filter := False;
        start_sel := combo.SelStart;
        self.combo.ItemIndex := 0;
        end_sel := length(self.combo.Text);
        self.combo.SelStart := start_sel;
        self.combo.SelLength := end_sel - start_sel;
        fdo_filter := True;
      end
      else
      begin
        self.combo.ItemIndex := -1;
      end;

    end;
  end;
end;

procedure mncombobox.refresh;
var
  i: integer;
  bk: TBookMark;
begin
  if not Assigned(dataset) then exit;
  bk := dataset.GetBookmark;
  dataset.DisableControls;
  dataset.First;
  fstring_array.Clear;
  fint_array.Clear;
  combo.Items.Clear;
  for i := 0 to self.dataset.RecordCount - 1 do
  begin
    fstring_array.add(dataset.FieldByName(list_field).AsString);
    combo.Items.Add(dataset.FieldByName(list_field).AsString);
    fint_array.add(dataset.FieldByName(id_field).AsInteger);
    dataset.Next;
  end;
  dataset.GotoBookmark(bk);
  dataset.EnableControls;
end;

{ mncustum_control }

procedure mncustum_control.on_paint(Sender: TObject);
var
  rct: trect;
  text_style: TTextStyle;
  acanvas: TCanvas;
  Caption: string;
begin
  if Sender is TGraphicControl then acanvas := TGraphicControl(Sender).Canvas
  else
    acanvas := TCustomControl(Sender).Canvas;

  if Assigned(on_paint_custom) then
    on_paint_custom(Sender);
  if Assigned(fon_paint_cach) then
    fon_paint_cach(Sender);




  aCanvas.Brush.Color :=
    TWinControl(Sender).parent.GetDefaultColor(dctBrush);
  acanvas.Brush.Style := TBrushStyle.bsSolid;
  acanvas.Pen.Style := psclear;
  acanvas.Fillrect(0, 0, TCustomControl(Sender).Width,
    TCustomControl(Sender).Height);

  acanvas.Brush.Color := clred;
  acanvas.Brush.Style := TBrushStyle.bsSolid;
  acanvas.Pen.Style := psclear;
  acanvas.Roundrect(0, 0, TCustomControl(Sender).Width,
    TCustomControl(Sender).Height, 20, 20);
  rct.Top := 0;
  rct.Left := 0;
  rct.Width := TCustomControl(Sender).Width;
  rct.Height := TCustomControl(Sender).Height;
  text_style.Alignment := taCenter;
  text_style.Layout := tlCenter;
  if assigned(bitmap) then
  begin
    acanvas.StretchDraw(rect(5, 5, 40, 40), bitmap);
  end;
  if Sender is TGraphicControl then caption  := TGraphicControl(Sender).Caption
  else
    caption := TCustomControl(Sender).caption;
  acanvas.Textrect(rct, 0, 0, Caption, text_style);

end;

procedure mncustum_control.init(control: TControl; bitmap: tbitmap;
  on_paint_custom: TNotifyEvent; color_brash: tcolor; color_font: tcolor;
  color_pen: tcolor; is_rounded: boolean);
begin
  fon_paint_cach := nil;
  self.on_paint_custom := on_paint_custom;
  self.bitmap := bitmap;
  self.alignemt_bitmap := TAlignment.taLeftJustify;
  self.alignemt_text := TAlignment.taLeftJustify;
  self.color_brash := color_brash;
  self.color_font := color_font;
  self.color_pen := color_pen;
  self.control := control;
  self.padding_bitmap := 3;
  self.is_rounded := is_rounded;
  if control is TCustomControl then
  begin
    if Assigned(TCustomControl(control).OnPaint) then
      self.fon_paint_cach := TCustomControl(control).OnPaint;
    TCustomControl(self.control).OnPaint := on_paint;
  end
  else
  begin
    if Assigned(TSpeedButton(control).OnPaint) then
      self.fon_paint_cach := TSpeedButton(control).OnPaint;
    TSpeedButton(self.control).OnPaint := on_paint;
  end;
end;

procedure mncustum_control.finalize;
begin
  self.bitmap.Free;
end;


function color_get_pascal_hex_from_web_hex(web_hex: string): string;
var
  i: integer;
begin
  Result := '';
  i := 0;
  if not IsLetterOrDigit(web_hex[1]) then i := 1;
  Result := Result + '$00';
  Result := Result + web_hex[i + 5];
  Result := Result + web_hex[i + 6];
  Result := Result + web_hex[i + 3];
  Result := Result + web_hex[i + 4];
  Result := Result + web_hex[i + 1];
  Result := Result + web_hex[i + 2];
end;

function dlg_confirmed(Value: string): boolean;
begin
  Result := MessageDlg(Value + ' ' + ('EST INTROUVABLE :VOULEZ L`AJOUTEZ?'),
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbCancel], 0) = mrYes;
end;

{ mneditor }

function mneditor.get_is_changed: boolean;
begin
  Result := (fold_value <> self.Text);
end;

function mneditor.get_text: string;
begin
  if control is TCustomMaskEdit then
    Result := TCustomMaskEdit(control).Text;
  if control is TCustomComboBox then
    Result := TCustomComboBox(control).Text;
end;

procedure mneditor.set_text(AValue: string);
begin
  if control is TCustomMaskEdit then
    TCustomMaskEdit(control).Text := avalue;
  if control is TCustomComboBox then
    TCustomComboBox(control).Text := AValue;
end;

procedure mneditor._on_exit(Sender: TObject);
begin
  if Assigned(on_validate) and is_changed then
  begin
    if control is TCustomMaskEdit then
      on_validate(control, TCustomMaskEdit(control).Text);
    if control is TCustomComboBox then
      on_validate(control, TCustomComboBox(control).Text);
  end;
end;

procedure mneditor._on_enter(Sender: TObject);
begin
  if control is TCustomMaskEdit then
    fold_value := TCustomMaskEdit(control).Text;
  if control is TCustomComboBox then
    fold_value := TCustomComboBox(control).Text;
end;

procedure mneditor._on_key_pressed(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin
    if Assigned(on_validate) and is_changed then
    begin
      if control is TCustomMaskEdit then
        on_validate(control, TCustomMaskEdit(control).Text);
      if control is TCustomComboBox then
        on_validate(control, TCustomComboBox(control).Text);
    end;
    key := #0;
  end;
end;


procedure mneditor.init(control: TWinControl; on_validate: ton_validate_notify);
begin
  self.control := control;
  self.on_validate := on_validate;
  if control is TCustomComboBox then
  begin
    TCustomComboBox(control).AutoCompleteText := [cbactEnabled];
    TCustomComboBox(control).AutoComplete := True;
  end;

  control.OnKeyPress := _on_key_pressed;
  control.OnExit := _on_exit;
  control.OnEnter := _on_enter;
end;

end.
