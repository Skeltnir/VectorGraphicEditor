unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UFloatPoint, UField;

type

  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create(AName: String);
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             virtual; abstract;
    procedure OnMouseMove_LB(APoint: TFloatPoint); virtual;
    procedure OnMouseClick_RB(APoint: TFloatPoint); virtual; abstract;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
                                                             override;
  end;


  { TPositioningTool }

  TPositioningTool = class(TTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint);override;
  end;


  { THandTool }

  THandTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint); override;
    public
      FTemp: TFloatPoint;
  end;


  { TMagnifierTool }

  TMagnifierTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint); override;
  end;

 var
   Tools: array [0..6] of TTool;
implementation

{ TMagnifierTool }

procedure TMagnifierTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle);
begin
  if (Field.FZoom < 8) and (Field.FZoom >= 1) then
    begin
      Field.FZoom += 1;
    end;
  if Field.FZoom < 1 then
    begin
      Field.FZoom *= 2;
    end;
end;

procedure TMagnifierTool.OnMouseClick_RB(APoint: TFloatPoint);
begin
  if Field.FZoom <= 1 then
    begin
      Field.FZoom /= 2;
    end;
  if Field.FZoom > 1 then
    begin
      Field.FZoom -= 1;
    end;
end;

{ TPositioningTool }

procedure TPositioningTool.OnMouseMove_LB(APoint: TFloatPoint);
begin

end;

procedure TPositioningTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle);
begin

end;

procedure TPositioningTool.OnMouseClick_RB(APoint: TFloatPoint);
begin

end;

{ THandTool }

procedure THandTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  Field.FShift.X += FTemp.X - APoint.X;
  Field.FShift.Y += FTemp.Y - APoint.Y;

end;

procedure THandTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle);
begin
  FTemp := APoint;
end;

procedure THandTool.OnMouseClick_RB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;APenStyle: TPenStyle);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, AWidth, APenStyle);
end;

{ TEllipseTool }

procedure TEllipseTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, AWidth, APenStyle);
end;

{ TRectangleTool }

procedure TRectangleTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, AWidth, APenStyle);
end;

{ TLineTool }

procedure TLineTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, AWidth, APenStyle);
end;

{ TPencilTool }

procedure TPencilTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, AWidth, APenStyle );
end;

 { TTool }

procedure TTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
   Figures[High(Figures)].AddPoint(APoint);
end;


constructor TTool.Create(AName: String);
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('Images\' + AName + '.bmp');
end;

initialization
 Tools[0] := TPencilTool.Create('pencil');
 Tools[1] := TLineTool.Create('line');
 Tools[2] := TRectangleTool.Create('rectangle');
 Tools[3] := TEllipseTool.Create('ellipse');
 Tools[4] := TRoundRectangleTool.Create('roundrect');
 Tools[5] := THandTool.Create('hand');
 Tools[6] := TMagnifierTool.Create('magnifier');
end.

