unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TFigure }

  TFigure = class
    constructor Create(APoint: TPoint; AWidth: Integer);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure AddPoint(APoint: TPoint); virtual; abstract;
    protected
      FWidth: Integer;
      FPoints: array of TPoint;
  end;

 { TPencil }

 TPencil = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TPoint); override;
 end;

 { TLine }

 TLine = class(TFigure)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TPoint); override;
 end;


 { TRectangle }

 TRectangle = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TPoint); override;
 end;


 { TEllipse }

 TEllipse = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TPoint); override;
 end;


 { TRoundRectangle }

 TRoundRectangle = class(TLine)
   procedure Draw(ACanvas: TCanvas); override;
   procedure AddPoint(APoint: TPoint); override;
 end;

 var
   Figures: array of TFigure;

implementation

{ TRoundRectangle }

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.MoveTo(FPoints[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.RoundRect(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y,50,100);
end;

procedure TRoundRectangle.AddPoint(APoint: TPoint);
begin
  FPoints[1] := APoint;
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  ACanvas.MoveTo(FPoints[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Ellipse(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

procedure TEllipse.AddPoint(APoint: TPoint);
begin
  FPoints[1] := APoint;
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.MoveTo(FPoints[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.Rectangle(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

procedure TRectangle.AddPoint(APoint: TPoint);
begin
  FPoints[1] := APoint
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
begin
  ACanvas.MoveTo(FPoints[0]);
  ACanvas.Pen.Width := FWidth;
  ACanvas.LineTo(FPoints[1]);
end;

procedure TLine.AddPoint(APoint: TPoint);
begin
  FPoints[1] := APoint;
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var i: integer;
begin
  ACanvas.MoveTo(FPoints[0]);
  ACanvas.Pen.Width := FWidth;
  for i := 1 to High(FPoints) do
      ACanvas.LineTo(FPoints[i]);
end;

procedure TPencil.AddPoint(APoint: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

{ TFigure }

constructor TFigure.Create(APoint: TPoint; AWidth: Integer);
begin
   SetLength(FPoints, 2);
   FPoints[0] := APoint;
   FPoints[1] := APoint;
   FWidth := AWidth;
end;

end.

