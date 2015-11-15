unit UFigures;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Graphics, UFloatPoint, UField;

type
  TScenePoints = array of TPoint;
  TFloatPoints = array[0..1] of TFloatPoint;
  { TFigure }

  TFigure = class
    constructor Create(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
                                                             APenColor: TColor;
                                                             ABrushColor: TColor);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure AddPoint(APoint: TFloatPoint); virtual; abstract;
    function ConvertToScene(APoints: array of TFloatPoint): TScenePoints;
    function GetBorders(APoints: array of TFloatPoint): TFloatPoints;
    private
      FRadiusX: Double;
      FRadiusY: Double;
      FPenStyle: TPenStyle;
      FWidth: Integer;
      FPenColor, FBrushColor: TColor;
      FBrushColorFlag: Boolean;
    public
      FPoints: array of TFloatPoint;
    published
        property PWidth: Integer read FWidth write FWidth;
        property PPenStyle: TPenStyle read FPenStyle write FPenStyle;
        property PPenColor: TColor read FPenColor write FPenColor;
  end;

 { TPencil }

 TPencil = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
 end;

 { TLine }

 TLine = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
 end;


 { TRectangle }

 TRectangle = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
 end;


 { TEllipse }

 TEllipse = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
 end;


 { TRoundRectangle }

 TRoundRectangle = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TFloatPoint); override;
 end;

 var
   Figures: array of TFigure;

implementation

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
  ACanvas.RoundRect(v[0].x, v[0].y, v[1].x, v[1].y,50,100);
end;

procedure TRoundRectangle.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
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
  ACanvas.Ellipse(v[0].x, v[0].y, v[1].x, v[1].y);
end;

procedure TEllipse.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
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
  ACanvas.Rectangle(v[0].x, v[0].y, v[1].x, v[1].y);
end;

procedure TRectangle.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint
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
  ACanvas.Brush.Color:= FBrushColor;
  ACanvas.LineTo(v[1]);
end;

procedure TLine.AddPoint(APoint: TFloatPoint);
begin
  FPoints[1] := APoint;
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
  ACanvas.Brush.Color:= FBrushColor;
  ACanvas.Polyline(v);
end;

procedure TPencil.AddPoint(APoint: TFloatPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

{ TFigure }

constructor TFigure.Create(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
   SetLength(FPoints, 2);
   FPoints[0] := APoint;
   FPoints[1] := APoint;
   FWidth := AWidth;
   FPenStyle := APenStyle;
   FPenColor := APenColor;
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

end.

