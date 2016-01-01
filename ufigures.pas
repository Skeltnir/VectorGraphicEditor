unit UFigures;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Graphics, UFloatPoint, UField, FPCanvas, LCLIntf, LCLType,
  typinfo;

type
  TScenePoints = array of TPoint;
  TFloatPoints = array[0..1] of TFloatPoint;

  TPenWidth  = integer;
  { TFigure }

  TFigure = class
    constructor Create(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure SelectedDraw(ACanvas: TCanvas); virtual; abstract;
    procedure AddPoint(APoint: TFloatPoint); virtual; abstract;
    function ConvertToScene(APoints: array of TFloatPoint): TScenePoints;
    function GetBorders(APoints: array of TFloatPoint): TFloatPoints;
    function isPointInside(APoint: TFloatPoint): Boolean; virtual; abstract;
    function isIntersected(ARect: TRect): Boolean; virtual; abstract;
    procedure MoveFigure(APoint: TFloatPoint);
    procedure ShowSelection(AValue: Boolean);
    procedure Load(AOpenStr: TStringList); virtual; abstract;
    function Save: string; virtual; abstract;
    private
      FSelected : Boolean;
      FBrushColor: TColor;
      FPenStyle: TFPPenStyle;
      FWidth: Integer;
      FPenColor, FBufColor: TColor;
    public
      FRgn: HRGN;
      FPoints: array of TFloatPoint;
    published
        property PPenWidth: Integer read FWidth write FWidth;
        property PPenStyle: TFPPenStyle read FPenStyle write FPenStyle;
        property isSelected: Boolean read FSelected write ShowSelection;
        property PPenColor: TColor read FPenColor write FPenColor;
        property PBrushColor: TColor read FBrushColor write FBrushColor;
  end;

  TFigures = class of TFigure;

 { TPencil }

 TPencil = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure SelectedDraw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
   function isPointInside(APoint: TFloatPoint): Boolean; override;
   function isIntersected(ARect: TRect): Boolean; override;
   function Save: string; override;
   procedure Load(AOpenStr: TStringList); override;
 end;

 { TLine }

 TLine = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure SelectedDraw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
   function isPointInside(APoint: TFloatPoint): Boolean; override;
   function isIntersected(ARect: TRect): Boolean; override;
   function Save: string; override;
   procedure Load(AOpenStr: TStringList); override;
 end;


 { TRectangle }

 TRectangle = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure SelectedDraw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
   function isPointInside(APoint: TFloatPoint): Boolean; override;
   function isIntersected(ARect: TRect): Boolean; override;
   function Save: string; override;
   procedure Load(AOpenStr: TStringList); override;
   private
     FBrushStyle: TBrushStyle;
   published
     property PBrushStyle: TBrushStyle read FBrushStyle
                                         write FBrushStyle;
 end;


 { TEllipse }

 TEllipse = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure SelectedDraw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
   function isPointInside(APoint: TFloatPoint): Boolean; override;
   function isIntersected(ARect: TRect): Boolean; override;
   function Save: string; override;
   procedure Load(AOpenStr: TStringList); override;
   private
     FBrushStyle: TBrushStyle;
   published
     property PBrushStyle: TBrushStyle read FBrushStyle
                                         write FBrushStyle;
 end;


 { TRoundRectangle }

 TRoundRectangle = class(TRectangle)
   procedure Draw(ACanvas: TCanvas); override;
   procedure SelectedDraw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
   function isPointInside(APoint: TFloatPoint): Boolean; override;
   function isIntersected(ARect: TRect): Boolean; override;
   function Save: string; override;
   procedure Load(AOpenStr: TStringList); override;
   private
     FRadiusX, FRadiusY: Integer;
   published
     property PRadiusX: Integer read FRadiusX write FRadiusX;
     property PRadiusY: Integer read FRadiusY write FRadiusY;
 end;

 var
   Figures: array of TFigure;
   FiguresName: array [0..4] of TFigure;
   Canva: ^TCanvas;

implementation

procedure MakeFramae(APoint1, APoint2: TFloatPoint);
begin

end;

{ TRoundRectangle }

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  ACanvas.MoveTo(v[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Brush.Color:= FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.RoundRect(v[0].x, v[0].y, v[1].x, v[1].y,FRadiusX,FRadiusY);
end;

procedure TRoundRectangle.SelectedDraw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  Self.Draw(ACanvas);
  v := ConvertToScene(FPoints);
  With ACanvas do
    begin
      Pen.Width := 2;
      Pen.Style := psDashDot;
      Brush.Style := bsClear;
      Pen.Color := clGreen;
    end;
  ACanvas.Rectangle(v[0].x - 4 - (FWidth div 2), v[0].y - 4 - (FWidth div 2),
        v[High(v)].x + 4 + (FWidth div 2), v[High(v)].y + 4 + (FWidth div 2));
end;

procedure TRoundRectangle.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
end;

function TRoundRectangle.isPointInside(APoint: TFloatPoint): Boolean;
var
  v: TScenePoints;
  p: TPoint;
begin
  Result := false;
  v := ConvertToScene(FPoints);
  p := Field.FieldToScene(APoint);
  FRgn := CreateRoundRectRgn(v[0].x - FWidth div 2, v[0].y - FWidth div 2,
  v[1].x + FWidth div 2, v[1].y + FWidth div 2,
                             FRadiusX, FRadiusY) ;
  isSelected := PtInRegion(FRgn, p.x, p.y);
  Result := FSelected;
  DeleteObject(FRgn);
end;

function TRoundRectangle.isIntersected(ARect: TRect): Boolean;
begin
  Result:=inherited isIntersected(ARect);
end;

function TRoundRectangle.Save: string;
var
  str: string;
  i: integer;
begin
  str := Self.ClassName + '{' + IntToStr(Length(FPoints)) + '{';
  for i := 0 to High(FPoints) do
    str += FloatToStr(FPoints[i].X) + ';' + FloatToStr(FPoints[i].Y)+'[';
  str += '{' + IntToStr(FPenColor);
  str += '{' + IntToStr(FBrushColor);
  str += '{' + IntToStr(FWidth);
  str += '{' + GetPropValue(Self, 'PPenStyle');
  str += '{' + GetPropValue(Self, 'PBrushStyle');
  str += '{' + IntToStr(FRadiusX);
  str += '{' + IntToStr(FRadiusY);
  Result := str;
end;

procedure TRoundRectangle.Load(AOpenStr: TStringList);
begin
  inherited Load(AOpenStr);
  FRadiusX := StrToInt(AOpenStr[8]);
  FRadiusY := StrToInt(AOpenStr[9]);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  ACanvas.MoveTo(v[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Brush.Color:= FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Ellipse(v[0].x, v[0].y, v[1].x, v[1].y);
end;

procedure TEllipse.SelectedDraw(ACanvas: TCanvas);
begin
  inherited SelectedDraw(ACanvas);
end;

procedure TEllipse.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
end;

function TEllipse.isPointInside(APoint: TFloatPoint): Boolean;
var
  v: TScenePoints;
  p: TPoint;
begin
  Result := false;
  v := ConvertToScene(FPoints);
  p := Field.FieldToScene(APoint);
  FRgn := CreateEllipticRgn(v[0].x - FWidth div 2, v[0].y - FWidth div 2,
  v[1].x + FWidth div 2, v[1].y + FWidth div 2) ;
  isSelected := PtInRegion(FRgn, p.x, p.y);
  Result := FSelected;
  DeleteObject(FRgn);
end;

function TEllipse.isIntersected(ARect: TRect): Boolean;
begin
  Result:=inherited isIntersected(ARect);
end;

function TEllipse.Save: string;
var
  str: string;
  i: integer;
begin
  str := Self.ClassName + '{' + IntToStr(Length(FPoints)) + '{';
  for i := 0 to High(FPoints) do
    str += FloatToStr(FPoints[i].X) + ';' + FloatToStr(FPoints[i].Y)+'[';
  str += '{' + IntToStr(FPenColor);
  str += '{' + IntToStr(FBrushColor);
  str += '{' + IntToStr(FWidth);
  str += '{' + GetPropValue(Self, 'PPenStyle');
  str += '{' + GetPropValue(Self, 'PBrushStyle');
  Result := str;
end;

procedure TEllipse.Load(AOpenStr: TStringList);
begin
  inherited Load(AOpenStr);
  SetPropValue(Self, 'PBrushStyle', AOpenStr[7]);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  ACanvas.MoveTo(v[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Brush.Color:= FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Rectangle(v[0].x, v[0].y, v[1].x, v[1].y);
end;

procedure TRectangle.SelectedDraw(ACanvas: TCanvas);
begin
  inherited SelectedDraw(ACanvas);
end;

procedure TRectangle.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint
end;

function TRectangle.isPointInside(APoint: TFloatPoint): Boolean;
var
  v: TScenePoints;
  p: TPoint;
begin
  Result := false;
  v := ConvertToScene(FPoints);
  p := Field.FieldToScene(APoint);
  FRgn := CreateRectRgn(v[0].x - FWidth div 2, v[0].y - FWidth div 2,
  v[1].x + FWidth div 2, v[1].y + FWidth div 2) ;
  isSelected := PtInRegion(FRgn, p.x, p.y);
  Result := FSelected;
  DeleteObject(FRgn);
end;

function TRectangle.isIntersected(ARect: TRect): Boolean;
var
  i: integer;
  v: TScenePoints;
  a: TRect;
begin
  v := ConvertToScene(FPoints);
  FRgn := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  a := Rect(v[0].x - 2, v[0].y - 2, v[1].x + 2, v[1].y + 2);
  isSelected := RectInRegion(FRGN, a) ;
  DeleteObject(FRgn);
  Result := FSelected;
end;

function TRectangle.Save: string;
var
  str: string;
  i: integer;
begin
  str := Self.ClassName + '{' + IntToStr(Length(FPoints)) + '{';
  for i := 0 to High(FPoints) do
    str += FloatToStr(FPoints[i].X) + ';' + FloatToStr(FPoints[i].Y)+'[';
  str += '{' + IntToStr(FPenColor);
  str += '{' + IntToStr(FBrushColor);
  str += '{' + IntToStr(FWidth);
  str += '{' + GetPropValue(Self, 'PPenStyle');
  str += '{' + GetPropValue(Self, 'PBrushStyle');
  Result := str;
end;

procedure TRectangle.Load(AOpenStr: TStringList);
begin
  inherited Load(AOpenStr);
  SetPropValue(Self, 'PBrushStyle', AOpenStr[7]);
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  ACanvas.MoveTo(v[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  ACanvas.LineTo(v[1]);
end;

procedure TLine.SelectedDraw(ACanvas: TCanvas);
var
  v: TScenePoints;
begin
  Draw(ACanvas);
  v := ConvertToScene(FPoints);
  Draw(ACanvas);
  With ACanvas do
    begin
      Pen.Width := 2;
      Pen.Style := psDashDot;
      Brush.Style := bsClear;
      Pen.Color := clGreen;
    end;
  ACanvas.Rectangle(v[0].x - 4 - (FWidth div 2), v[0].y - 4 - (FWidth div 2),
        v[High(v)].x + 4 + (FWidth div 2), v[High(v)].y + 4 + (FWidth div 2));
end;

procedure TLine.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
end;

function TLine.isPointInside(APoint: TFloatPoint): Boolean;
var
  v: TScenePoints;
  p: TPoint;
begin
  v := ConvertToScene(FPoints);
  p := Field.FieldToScene(APoint);
  FRgn := CreateRectRgn(v[0].x - 3, v[0].y - 3, v[1].x + 3, v[1].y + 3) ;
  isSelected := PtInRegion(FRgn, p.x, p.y);
  Result := FSelected;
  DeleteObject(FRgn);
end;

function TLine.isIntersected(ARect: TRect): Boolean;
var
  i: integer;
  v: TScenePoints;
  a: TRect;
begin
  v := ConvertToScene(FPoints);
  FRgn := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  a := Rect(v[0].x - 2, v[0].y - 2, v[1].x + 2, v[1].y + 2);
  isSelected := RectInRegion(FRGN, a) ;
  DeleteObject(FRgn);
  Result := FSelected;
end;

function TLine.Save: string;
var
  str: string;
  i: integer;
begin
  str := ClassName + '{' + IntToStr(Length(FPoints)) + '{';
  for i := 0 to High(FPoints) do
    str += FloatToStr(FPoints[i].X) + ';' + FloatToStr(FPoints[i].Y)+'[';
  str += '{' + IntToStr(FPenColor);
  str += '{' + IntToStr(FBrushColor);
  str += '{' + IntToStr(FWidth);
  str += '{' + GetPropValue(Self, 'PPenStyle');
  Result := str;
end;

procedure TLine.Load(AOpenStr: TStringList);
var
  i, index: integer;
  points, XY: TStringList;
  style: String;
begin
  SetLength(FPoints, StrToInt(AOpenStr[1]));
  points := TStringList.Create;
  points.Delimiter := '[';
  points.DelimitedText := AOpenStr[2];
  for i := 0 to High(FPoints) do
    begin
      XY := TStringList.Create;
      XY.Delimiter := ';';
      XY.DelimitedText := Points[i];
      FPoints[i].X := StrToFloat(XY.Strings[0]);
      FPoints[i].Y := StrToFloat(XY.Strings[1]);
      XY.Destroy;
    end;
  FPenColor := StrToInt(AOpenStr[3]);
  FBrushColor := StrToInt(AOpenStr[4]);
  FWidth := StrToInt(AOpenStr[5]);
  SetPropValue(Self, 'PPenStyle', AOpenStr[6])
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var i: integer;
    v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  ACanvas.MoveTo(v[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Polyline(v);
  {for i := 0 to High(v) do
    begin
      ACanvas.LineTo(v[i]);
    end;}
end;

procedure TPencil.SelectedDraw(ACanvas: TCanvas);
var
    v: TScenePoints;
begin
  v := ConvertToScene(FPoints);
  Draw(ACanvas);
  With ACanvas do
    begin
      Pen.Width := 2;
      Pen.Style := psDashDot;
      Brush.Style := bsClear;
      Pen.Color := clGreen;
    end;
  ACanvas.Rectangle(v[0].x - 1, v[0].y - 1, v[High(v)].x + 1, v[High(v)].y + 1);
end;

procedure TPencil.AddPoint(APoint: TFloatPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

function TPencil.isPointInside(APoint: TFloatPoint): Boolean;
var
  i: integer;
  v: TScenePoints;
  a: TRect;
begin
  Result := false;
  v := ConvertToScene(FPoints);
  a := Rect(round(APoint.X-3), round(APoint.Y-3), round(APoint.X+3), round(APoint.Y+3));
  for i := 0 to High(v) do
    begin
      if (v[i].X >= a.Left) and (v[i].X <= a.Right) then
        if (v[i].Y >= a.Top) and (v[i].Y <= a.Bottom) then
          begin
            isSelected := True;
            Result := FSelected;
            break;
          end
      else
        begin
          isSelected := False;
          Result := FSelected;
        end;
    end;
  DeleteObject(FRgn);
end;

function TPencil.isIntersected(ARect: TRect): Boolean;
var
  i: integer;
  v: TScenePoints;
  p: ^TScenePoints;
begin
  v := ConvertToScene(FPoints);
  for i := 0 to High(v) do
    begin
      if (v[i].X > ARect.Left) and (v[i].X < ARect.Right) then
        begin
        if (v[i].Y > ARect.Top) and (v[i].Y < ARect.Bottom) then
          begin
            isSelected := True;
            Result := FSelected;
            break;
          end
      else
          begin
            isSelected := False;
            Result := FSelected;
          end;
        end
      else
        begin
          isSelected := False;
          Result := FSelected;
        end;
    end;
end;

function TPencil.Save: string;
var
  str: string;
  i: integer;
begin
  str := Self.ClassName + '{' + IntToStr(Length(FPoints)) + '{';
  for i := 0 to High(FPoints) do
    str += FloatToStr(FPoints[i].X) + ';' + FloatToStr(FPoints[i].Y)+'[';
  str += '{' + IntToStr(FPenColor);
  str += '{' + IntToStr(FBrushColor);
  str += '{' + IntToStr(FWidth);
  str += '{' + GetPropValue(Self, 'PPenStyle');
  Result := str;
end;

procedure TPencil.Load(AOpenStr: TStringList);
var
  i, index: integer;
  points, XY: TStringList;
  style: String;
begin
  SetLength(FPoints, StrToInt(AOpenStr[1]));
  points := TStringList.Create;
  points.Delimiter := '[';
  points.DelimitedText := AOpenStr[2];
  for i := 0 to High(FPoints) do
    begin
      XY := TStringList.Create;
      XY.Delimiter := ';';
      XY.DelimitedText := Points[i];
      FPoints[i].X := StrToFloat(XY.Strings[0]);
      FPoints[i].Y := StrToFloat(XY.Strings[1]);
      XY.Destroy;
    end;
  FPenColor := StrToInt(AOpenStr[3]);
  FBrushColor := StrToInt(AOpenStr[4]);
  FWidth := StrToInt(AOpenStr[5]);
  SetPropValue(Self, 'PPenStyle', AOpenStr[6])
end;

{ TFigure }

constructor TFigure.Create(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
begin
   SetLength(FPoints, 2);
   FPoints[0] := APoint;
   FPoints[1] := APoint;
   FPenColor := APenColor;
   FBufColor := APenColor;
   FBrushColor := ABrushColor;
end;

function TFigure.ConvertToScene(APoints: array of TFloatPoint): TScenePoints;
var
  a: TScenePoints;
  i: integer;
begin
  SetLength(a, Length(FPoints));
  for i := 0 to High(a) do
    begin
      a[i] := Field.FieldToScene(FPoints[i]);
    end;
  ConvertToScene := a;
end;

function TFigure.GetBorders(APoints: array of TFloatPoint): TFloatPoints;
var
  MaxX, MaxY, MinX, MinY: Double;
  i: integer;
begin
  MaxX := APoints[0].X;
  MinX := APoints[0].X;
  MaxY := APoints[0].Y;
  MinY := APoints[0].Y;
  for i := 0 to High(APoints) do
    begin
      if MaxX < APoints[i].X then MaxX := APoints[i].X;
      if MaxY < APoints[i].Y then MaxY := APoints[i].Y;
      if MinX > APoints[i].X then MinX := APoints[i].X;
      if MinY > APoints[i].Y then MinY := APoints[i].Y;
    end;
  Result[0].X := MaxX;
  Result[0].Y := MaxY;
  Result[1].X := MinX;
  Result[1].Y := MinY;
end;

procedure TFigure.MoveFigure(APoint: TFloatPoint);
var
  i: integer;
begin
  for i := 0 to High(FPoints) do
    begin
      FPoints[i].X += (APoint.X);
      Fpoints[i].Y += (APoint.Y);
    end;
end;

procedure TFigure.ShowSelection(AValue: Boolean);
begin
  if AValue then
    begin
      FSelected := True;
    end
  else
    begin
      FSelected := False;
    end;
end;

initialization

end.

