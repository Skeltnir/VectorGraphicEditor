unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UFloatPoint, UField;

type
  TScenePoints = array of TPoint;
  { TFigure }

  TFigure = class
    constructor Create(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure AddPoint(APoint: TFloatPoint); virtual; abstract;
    function ConvertToScene(APoints: array of TFloatPoint): TScenePoints;
    protected
      FPenStyle: TPenStyle;
      FWidth: Integer;
      FPoints: array of TFloatPoint;
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
  ACanvas.Polyline(v);
end;

procedure TPencil.AddPoint(APoint: TFloatPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

{ TFigure }

constructor TFigure.Create(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
begin
   SetLength(FPoints, 2);
   FPoints[0] := APoint;
   FPoints[1] := APoint;
   FWidth := AWidth;
   FPenStyle := APenStyle;
end;

function TFigure.ConvertToScene(APoints: array of TFloatPoint): TScenePoints;
var
  x: TFloatPoint;
  a: TScenePoints;
begin
  for x in FPoints do
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := Field.FieldTOScene(x);
    end;
  ConvertToScene := a;
end;

end.

