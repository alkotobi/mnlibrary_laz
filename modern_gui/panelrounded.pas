unit PanelRounded;

{$mode delphi}{$H+}

interface

uses
  Buttons, ComCtrls, StdCtrls, Classes, SysUtils, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, LMessages;

type

  { TPanelRounded }
  TAlignementHorisontal = (alTop, alMidle, AlBottum);
  TAlignementVertical = (alRight, alCenter, AlLeft);

  { TPadding }

  TPadding = class
    top, buttom, left, right: integer;
  end;




type
  TPanelRounded = class(TPanel)
  private
    FcaptionAlignement: TAlignment;
    FcolorBk: TColor;
    FcolorBkClicked: TColor;
    FcolorBKHover: TColor;
    FcolorBorder: TColor;
    FcolorBorderClicked: TColor;
    FcolorBorderHover: TColor;
    FcontrolBounded: twincontrol;
    FhasBorder: boolean;
    Ficon: tbitmap;
    FiconHorAlign: TAlignementHorisontal;
    FiconHeight: integer;
    FiconLeft: integer;
    FiconPadding: TPadding;
    FiconTop: integer;
    FiconVerAlign: TAlignementVertical;
    FiconWidth: integer;
    FimageIndex: integer;
    FimageList: TImageList;
    FImageIndexClicked: integer;
    FImageIndexHover: integer;
    FisClickable: boolean;
    FisHooverAble: boolean;
    Froundx: integer;
    Froundy: integer;
    FBorderWidth: integer;
    FIsclicked: boolean;
    FIsMouseInter: boolean;
    function getColor: TColor;
    function GetIcon: tbitmap;
    procedure SetcaptionAlignement(AValue: TAlignment);
    procedure SetcolorBk(AValue: TColor);
    procedure SetcolorBkClicked(AValue: TColor);
    procedure SetcolorBKHover(AValue: TColor);
    procedure SetcolorBorder(AValue: TColor);
    procedure SetcolorBorderClicked(AValue: TColor);
    procedure SetcolorBorderHover(AValue: TColor);
    procedure SetcontrolBounded(AValue: twincontrol);
    procedure SethasBorder(AValue: boolean);
    procedure Seticon(AValue: tbitmap);
    procedure SeticonHorAlign(AValue: TAlignementHorisontal);
    procedure SeticonHeight(AValue: integer);
    procedure SeticonLeft(AValue: integer);
    procedure SeticonPadding(AValue: TPadding);
    procedure SeticonTop(AValue: integer);
    procedure SeticonVerAlign(AValue: TAlignementVertical);
    procedure SeticonWidth(AValue: integer);
    procedure SetimageIndex(AValue: integer);
    procedure SetimageList(AValue: TImageList);
    procedure SetImageIndexClicked(AValue: integer);
    procedure SetImageIndexHover(AValue: integer);
    procedure SetisClickable(AValue: boolean);
    procedure SetisHooverAble(AValue: boolean);
    procedure Setroundx(AValue: integer);
    procedure Setroundy(AValue: integer);
    procedure SetBorderWidth(AValue: integer);
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
      Shift: TShiftState); override;
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Click; override;
  protected
    procedure Paint; override;
    property icon: tbitmap read GetIcon write Seticon;
    procedure DoOnChangeBounds; override;
    property color;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property imageList: TImageList read FimageList write SetimageList;
    property imageIndex: integer read FimageIndex write SetimageIndex;
    property colorBk: TColor read FcolorBk write SetcolorBk;
    property colorBorder: TColor read FcolorBorder write SetcolorBorder;
    property hasBorder: boolean read FhasBorder write SethasBorder;
    property captionAlignement: TAlignment read FcaptionAlignement
      write SetcaptionAlignement;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property iconHorAlign: TAlignementHorisontal
      read FiconHorAlign write SeticonHorAlign;
    property iconVerAlign: TAlignementVertical read FiconVerAlign write SeticonVerAlign;
    property iconWidth: integer read FiconWidth write SeticonWidth;
    property iconHeight: integer read FiconHeight write SeticonHeight;
    property iconTop: integer read FiconTop write SeticonTop;
    property iconLeft: integer read FiconLeft write SeticonLeft;
    property controlBounded: twincontrol read FcontrolBounded write SetcontrolBounded;
    property roundx: integer read Froundx write Setroundx;
    property roundy: integer read Froundy write Setroundy;
    property colorBkClicked: TColor read FcolorBkClicked write SetcolorBkClicked;
    property ImageIndexClicked: integer read FImageIndexClicked
      write SetImageIndexClicked;
    property colorBKHover: TColor read FcolorBKHover write SetcolorBKHover;
    property colorBorderHover: TColor read FcolorBorderHover write SetcolorBorderHover;
    property colorBorderClicked: TColor read FcolorBorderClicked
      write SetcolorBorderClicked;
    property ImageIndexHover: integer read FImageIndexHover write SetImageIndexHover;
    property isClickable: boolean read FisClickable write SetisClickable;
    property isHooverAble: boolean read FisHooverAble write SetisHooverAble;
  end;



procedure Register;

implementation

procedure Register;
begin
  {$I panelrounded_icon.lrs}
  RegisterComponents('Additional', [TPanelRounded]);
end;


{ TPanelRounded }

procedure TPanelRounded.SetimageList(AValue: TImageList);
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

procedure TPanelRounded.SetImageIndexClicked(AValue: integer);
begin
  if FImageIndexClicked = AValue then Exit;
  FImageIndexClicked := AValue;
end;

procedure TPanelRounded.SetImageIndexHover(AValue: integer);
begin
  if FImageIndexHover = AValue then Exit;
  FImageIndexHover := AValue;
end;

procedure TPanelRounded.SetisClickable(AValue: boolean);
begin
  if FisClickable = AValue then Exit;
  FisClickable := AValue;
end;

procedure TPanelRounded.SetisHooverAble(AValue: boolean);
begin
  if FisHooverAble = AValue then Exit;
  FisHooverAble := AValue;
end;

procedure TPanelRounded.Setroundx(AValue: integer);
begin
  if Froundx = AValue then Exit;
  Froundx := AValue;
  self.Repaint;
end;

procedure TPanelRounded.Setroundy(AValue: integer);
begin
  if Froundy = AValue then Exit;
  Froundy := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth = AValue then Exit;
  FBorderWidth := AValue;
  self.Repaint;
end;


procedure TPanelRounded.DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
  Shift: TShiftState);
begin
  inherited DoMouseDown(Message, Button, Shift);
  FIsclicked := True;
  repaint;
end;

procedure TPanelRounded.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  inherited DoMouseUp(Message, Button);
  FIsclicked := False;
  repaint
end;




procedure TPanelRounded.MouseEnter;
begin
  inherited MouseEnter;
  FIsMouseInter := True;
  repaint
end;

procedure TPanelRounded.MouseLeave;
begin
  inherited MouseLeave;
  FIsMouseInter := False;
  repaint;
end;

procedure TPanelRounded.Click;
begin
  if isClickable then
    inherited Click;
  repaint;
end;


procedure TPanelRounded.Paint;
var
  rct: trect;
  text_style: TTextStyle;
begin
  inherited Paint;
  //paint the packround color of parent
  self.Canvas.Brush.Color :=self.parent.GetDefaultColor(dctBrush);
  self.Canvas.Brush.Style := TBrushStyle.bsSolid;
  self.Canvas.Pen.Style := psclear;
  self.Canvas.Fillrect(0, 0, self.Width,self.Height);

   //paint the packround
  if FIsclicked and isClickable then
  begin
    self.Canvas.Brush.Color := colorBkClicked;
  end
  else if FIsMouseInter and isHooverAble then
  begin
    self.Canvas.Brush.Color := colorBKHover;
  end
  else
    self.Canvas.Brush.Color := colorBk;
  self.Canvas.Brush.Style := TBrushStyle.bsSolid;
  if FIsclicked and isClickable then
  begin
    self.Canvas.Pen.Color := colorBorderClicked;
  end
  else if FIsMouseInter and isHooverAble then
  begin
    self.Canvas.Pen.Color := colorBorderHover;
  end
  else
    self.Canvas.Pen.Color := colorBorder;
  self.Canvas.Pen.Width := BorderWidth;
  Canvas.Pen.JoinStyle := TPenJoinStyle.pjsRound;
  if not hasBorder then
    self.Canvas.Pen.Style := psclear
  else
    self.Canvas.Pen.Style := psSolid;
  self.Canvas.Roundrect(0, 0, TCustomControl(self).Width,
    TCustomControl(self).Height, roundx, roundy);

  rct.Top := 0;
  rct.Left := 0;
  rct.Width := TCustomControl(self).Width;
  rct.Height := TCustomControl(self).Height;
  text_style.Alignment := taCenter;
  text_style.Layout := tlCenter;
  if assigned(imageList) then
  begin
    if FIsclicked and isClickable then
    begin
      imageList.GetBitmap(ImageIndexClicked, Ficon);
    end
    else if FIsMouseInter and isHooverAble then
    begin
      imageList.GetBitmap(ImageIndexHover, Ficon);
    end
    else
      imageList.GetBitmap(imageIndex, Ficon);
    self.Canvas.StretchDraw(Bounds(iconleft, iconTop, iconWidth, iconHeight), ficon);
  end;
  self.Canvas.Textrect(rct, 0, 0, Caption, text_style);
end;

procedure TPanelRounded.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  if assigned(FcontrolBounded) then
    FcontrolBounded.BoundsRect :=
      Bounds(self.Left + 1, self.Top + 1, self.Width - 1, self.Height - 1);

end;

constructor TPanelRounded.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FimageList := nil;
  FimageIndex := -1;
  ficon := tbitmap.Create;
  font.Color := clBlack;
  fcolorBk := clgreen;
  color := clgreen;
  fcolorBorder := clBlack;
  FcolorBkClicked := fcolorBk;
  FcolorBKHover := fcolorBk;
  FcolorBorderClicked := fcolorBorder;
  FcolorBorderHover := fcolorBorder;
  FImageIndexClicked := -1;
  FImageIndexHover := -1;
  FisClickable := False;
  FisHooverAble := False;
  FhasBorder := True;
  FBorderWidth := 1;
  FiconHorAlign := TAlignementHorisontal.alTop;
  FiconVerAlign := TAlignementVertical.AlLeft;
  FiconWidth := 50;
  FiconHeight := 50;
  FiconTop := 5;
  FiconLeft := 5;
  Froundx := 20;
  Froundy := 20;
end;

destructor TPanelRounded.Destroy;
begin
  inherited Destroy;
  if Assigned(ficon) then
    Ficon.Free;
end;

procedure TPanelRounded.SetimageIndex(AValue: integer);
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

procedure TPanelRounded.Seticon(AValue: tbitmap);
begin
  if Ficon = AValue then Exit;
  Ficon := AValue;
end;

procedure TPanelRounded.SeticonHorAlign(AValue: TAlignementHorisontal);
begin
  if FiconHorAlign = AValue then Exit;
  FiconHorAlign := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SeticonHeight(AValue: integer);
begin
  if FiconHeight = AValue then Exit;
  FiconHeight := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SeticonLeft(AValue: integer);
begin
  if FiconLeft = AValue then Exit;
  FiconLeft := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SeticonPadding(AValue: TPadding);
begin
  if FiconPadding = AValue then Exit;
  FiconPadding := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SeticonTop(AValue: integer);
begin
  if FiconTop = AValue then Exit;
  FiconTop := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SeticonVerAlign(AValue: TAlignementVertical);
begin
  if FiconVerAlign = AValue then Exit;
  FiconVerAlign := AValue;
end;

procedure TPanelRounded.SeticonWidth(AValue: integer);
begin
  if FiconWidth = AValue then Exit;
  FiconWidth := AValue;
  self.Repaint;
end;

function TPanelRounded.GetIcon: tbitmap;
begin
  Result := ficon;
end;

function TPanelRounded.getColor: TColor;
begin
  Result := colorBk;
end;


procedure TPanelRounded.SetcaptionAlignement(AValue: TAlignment);
begin
  if FcaptionAlignement = AValue then Exit;
  FcaptionAlignement := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SetcolorBk(AValue: TColor);
begin
  if FcolorBk = AValue then Exit;
  FcolorBk := AValue;
  self.Color := AValue;
  self.Repaint;
end;

procedure TPanelRounded.SetcolorBkClicked(AValue: TColor);
begin
  if FcolorBkClicked = AValue then Exit;
  FcolorBkClicked := AValue;
  Repaint;
end;

procedure TPanelRounded.SetcolorBKHover(AValue: TColor);
begin
  if FcolorBKHover = AValue then Exit;
  FcolorBKHover := AValue;
  Repaint;
end;

procedure TPanelRounded.SetcolorBorder(AValue: TColor);
begin
  if FcolorBorder = AValue then Exit;
  FcolorBorder := AValue;
  Self.Repaint;
end;

procedure TPanelRounded.SetcolorBorderClicked(AValue: TColor);
begin
  if FcolorBorderClicked = AValue then Exit;
  FcolorBorderClicked := AValue;
  Repaint;
end;

procedure TPanelRounded.SetcolorBorderHover(AValue: TColor);
begin
  if FcolorBorderHover = AValue then Exit;
  FcolorBorderHover := AValue;
  repaint;
end;

procedure TPanelRounded.SetcontrolBounded(AValue: twincontrol);
begin
  if FcontrolBounded = AValue then Exit;
  FcontrolBounded := AValue;
  if assigned(FcontrolBounded) then
  begin
    FcontrolBounded.BoundsRect :=
      Bounds(self.Left + 3, self.Top + 3, self.Width - 3, self.Height - 3);
    FcontrolBounded.Color := self.colorBk;
  end;
end;


procedure TPanelRounded.SethasBorder(AValue: boolean);
begin
  if FhasBorder = AValue then Exit;
  FhasBorder := AValue;
  self.Repaint;
end;




end.
