unit SpeedbuttonRounded;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LResources, Forms, Controls, Graphics, Dialogs, Buttons;

type

  { TSpeedbuttonRounded }

  TSpeedbuttonRounded = class(TSpeedButton)
  private
    FcaptionAlignement: TAlignment;
    //FColor: TColor;
    FcolorBk: TColor;
    FcolorBorder: TColor;
    FhasStrock: boolean;
    Ficon: tbitmap;
    FiconHeight: integer;
    FiconLeft: integer;
    FiconTop: integer;
    FiconWidth: integer;
    FimageIndex: integer;
    FimageList: TImageList;
    Froundx: integer;
    Froundy: integer;
    FStrockWidth: integer;
    fisMouseClick: boolean;
    //function getColor: TColor;
    function GetIcon: tbitmap;
    procedure SetcaptionAlignement(AValue: TAlignment);
    procedure SetcolorBk(AValue: TColor);
    procedure SetcolorBorder(AValue: TColor);
    procedure SethasStrock(AValue: boolean);
    procedure Seticon(AValue: tbitmap);
    procedure SeticonHeight(AValue: integer);
    procedure SeticonLeft(AValue: integer);
    procedure SeticonTop(AValue: integer);
    procedure SeticonWidth(AValue: integer);
    procedure SetimageIndex(AValue: integer);
    procedure SetimageList(AValue: TImageList);
    procedure Setroundx(AValue: integer);
    procedure Setroundy(AValue: integer);
    procedure SetStrockWidth(AValue: integer);
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton); override;
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton; Shift: TShiftState
      ); override;
  protected
    procedure Paint; override;
    property icon: tbitmap read GetIcon write Seticon;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property imageList: TImageList read FimageList write SetimageList;
    property imageIndex: integer read FimageIndex write SetimageIndex;
    property colorBk: TColor read FcolorBk write SetcolorBk;
    property colorBorder: TColor read FcolorBorder write SetcolorBorder;
    property hasStrock: boolean read FhasStrock write SethasStrock;
    property captionAlignement: TAlignment read FcaptionAlignement
      write SetcaptionAlignement;
    property StrockWidth: integer read FStrockWidth write SetStrockWidth;
    property iconWidth: integer read FiconWidth write SeticonWidth;
    property iconHeight: integer read FiconHeight write SeticonHeight;
    property iconTop: integer read FiconTop write SeticonTop;
    property iconLeft: integer read FiconLeft write SeticonLeft;
    property roundx: integer read Froundx write Setroundx;
    property roundy: integer read Froundy write Setroundy;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I speedbuttonrounded_icon.lrs}
  RegisterComponents('Additional', [TSpeedbuttonRounded]);
end;

{ TSpeedbuttonRounded }

//function TSpeedbuttonRounded.getColor: TColor;
//begin
//  Result := FcolorBk;
//end;

function TSpeedbuttonRounded.GetIcon: tbitmap;
begin
  Result := Ficon;
end;

procedure TSpeedbuttonRounded.SetcaptionAlignement(AValue: TAlignment);
begin
  FcaptionAlignement := AValue;
  self.Repaint;
end;

procedure TSpeedbuttonRounded.SetcolorBk(AValue: TColor);
begin
  FcolorBk := AValue;
  color := AValue;
  self.Repaint;
end;

procedure TSpeedbuttonRounded.SetcolorBorder(AValue: TColor);
begin
  FcolorBorder := AValue;
  Repaint;
end;



procedure TSpeedbuttonRounded.SethasStrock(AValue: boolean);
begin
  FhasStrock := AValue;
  Repaint;
end;

procedure TSpeedbuttonRounded.Seticon(AValue: tbitmap);
begin
  Ficon := AValue;
  Repaint;
end;

procedure TSpeedbuttonRounded.SeticonHeight(AValue: integer);
begin
  FiconHeight := AValue;
  Repaint;
end;



procedure TSpeedbuttonRounded.SeticonLeft(AValue: integer);
begin
  FiconLeft := AValue;
  Repaint;
end;



procedure TSpeedbuttonRounded.SeticonTop(AValue: integer);
begin
  FiconTop := AValue;
  Repaint;
end;


procedure TSpeedbuttonRounded.SeticonWidth(AValue: integer);
begin
  FiconWidth := AValue;
  Repaint;
end;

procedure TSpeedbuttonRounded.SetimageIndex(AValue: integer);
begin
  if FimageIndex = AValue then Exit;
  FimageIndex := AValue;
  if FimageIndex >= 0 then
  begin
    if (assigned(imageList)) then
    begin
      imageList.GetBitmap(AValue, icon);
    end;
  end
  else
  begin
    icon.Clear;
  end;
  self.Repaint;
end;

procedure TSpeedbuttonRounded.SetimageList(AValue: TImageList);
begin
  if FimageList = AValue then Exit;
  FimageList := AValue;
  if FimageIndex > -1 then
  begin
    if assigned(FimageList) then
      fimageList.GetBitmap(FimageIndex, ficon)
    else
    begin
      ficon.Clear;
    end;
  end;
  self.Repaint;
end;

procedure TSpeedbuttonRounded.Setroundx(AValue: integer);
begin
  if Froundx = AValue then Exit;
  Froundx := AValue;
  Repaint;
end;

procedure TSpeedbuttonRounded.Setroundy(AValue: integer);
begin
  if Froundy = AValue then Exit;
  Froundy := AValue;
  Repaint;
end;

procedure TSpeedbuttonRounded.SetStrockWidth(AValue: integer);
begin
  FStrockWidth := AValue;
  Repaint;
end;



procedure TSpeedbuttonRounded.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  inherited DoMouseUp(Message, Button);
  fisMouseClick := false;
end;

procedure TSpeedbuttonRounded.DoMouseDown(var Message: TLMMouse;
  Button: TMouseButton; Shift: TShiftState);
begin
  inherited DoMouseDown(Message, Button, Shift);
    fisMouseClick := true;
end;


procedure TSpeedbuttonRounded.Paint;
var
  rct: trect;
  text_style: TTextStyle;
begin
  //inherited Paint;

  self.Canvas.Brush.Color :=
    self.parent.GetDefaultColor(dctBrush);
  self.Canvas.Brush.Style := TBrushStyle.bsSolid;
  self.Canvas.Pen.Style := psclear;
  self.Canvas.Fillrect(0, 0, self.Width,
    self.Height);
  if FState=bsHot then
    self.Canvas.Brush.Color := clYellow
  else if fisMouseClick then
  begin
    self.Canvas.Brush.Color := clred;
  end

  else
    self.Canvas.Brush.Color := colorBk;
  self.Canvas.Brush.Style := TBrushStyle.bsSolid;
  self.Canvas.Pen.Color := colorBorder;
  Canvas.Pen.JoinStyle:=TPenJoinStyle.pjsRound;
  self.Canvas.Pen.Width := 7;
  self.Canvas.Pen.Width := StrockWidth;
  if not hasStrock then
    self.Canvas.Pen.Style := psclear
  else
    self.Canvas.Pen.Style := psSolid;
  self.Canvas.Roundrect(0, 0, self.Width,
    self.Height, roundx, roundy);
  rct.Top := 0;
  rct.Left := 0;
  rct.Width := self.Width;
  rct.Height := self.Height;
  text_style.Alignment := taCenter;
  text_style.Layout := tlCenter;
  if assigned(ficon) then
  begin
    self.Canvas.StretchDraw(Bounds(iconleft, iconTop, iconWidth, iconHeight), ficon);
  end;
  self.Canvas.Textrect(rct, 0, 0, Caption, text_style);
end;

constructor TSpeedbuttonRounded.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FimageList := nil;
  FimageIndex := -1;
  ficon := tbitmap.Create;
  font.Color := clBlack;
  fcolorBk := clgreen;
  color := clgreen;
  fcolorBorder := clBlack;
  FhasStrock := True;
  FStrockWidth := 1;
  FiconWidth := 30;
  FiconHeight := 30;
  FiconTop := 5;
  FiconLeft := 5;
  Froundx := 20;
  Froundy := 20;
  fisMouseClick := False;
end;

destructor TSpeedbuttonRounded.Destroy;
begin
  if Assigned(ficon) then
    ficon.Free;
  inherited Destroy;
end;

end.
