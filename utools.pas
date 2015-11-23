unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UFloatPoint, UField, TypInfo,
  ExtCtrls, StdCtrls, UEditors, FPCanvas;

type

   TTools = class of TTool;
  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create();
    procedure OnMouseClick_LB(APoint: TFloatPoint;
          APenColor: TColor; ABrushColor: TColor); virtual; abstract;
    procedure OnMouseMove_LB(APoint: TFloatPoint); virtual;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);
                                                  virtual; abstract;
    procedure OnMouseMove_RB(APoint: TFloatPoint); virtual;
    function GetFigureProp(): integer;virtual; abstract;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer; override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer;override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer;override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    function GetFigureProp: integer; override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    function GetFigureProp: integer; override;
  end;


  { TPositioningTool }

  TPositioningTool = class(TTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
  end;


  { THandTool }

  THandTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
    public
      FTemp: TFloatPoint;
  end;


  { TMagnifierTool }

  TMagnifierTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
  end;


  procedure RegisterTool(ATool: TTools);




 var
   Tools: array of TTool;
   Editors: array of TEditor;
   PropList: PPropList;

implementation

{ TMagnifierTool }

procedure TMagnifierTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseClick_LB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  Field.FShift.X := APoint.X / Field.FZoom;
  Field.FShift.Y := APoint.Y / Field.FZoom;
  if (Field.FZoom < 8) and (Field.FZoom >= 1) then
    begin
      Field.FZoom += 1;
    end;
  if Field.FZoom < 1 then
    begin
      Field.FZoom *= 2;
    end;
  Field.FShift.X := (Field.FShift.X - APoint.X * Field.FZoom)/ (-(Field.FZoom + 1));
  Field.FShift.Y := (Field.FShift.Y - APoint.Y * Field.FZoom)/(-(Field.FZoom + 1));
end;

procedure TMagnifierTool.OnMouseClick_RB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  Field.FShift.X := Field.FieldToScene(APoint).X / Field.FZoom;
  Field.FShift.Y := Field.FieldToScene(APoint).Y / Field.FZoom;
  if Field.FZoom <= 1 then
    begin
      Field.FZoom /= 2;
    end;
  if Field.FZoom > 1 then
    begin
      Field.FZoom -= 1;
    end;
  Field.FShift.X := (Field.FShift.X - APoint.X * Field.FZoom)/ (-(Field.FZoom + 1));
  Field.FShift.Y := (Field.FShift.Y - APoint.Y * Field.FZoom)/ (-(Field.FZoom + 1));
end;

procedure TMagnifierTool.OnMouseMove_RB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

{ TPositioningTool }

procedure TPositioningTool.OnMouseMove_LB(APoint: TFloatPoint);
begin

end;

procedure TPositioningTool.OnMouseClick_LB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TPositioningTool.OnMouseClick_RB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TPositioningTool.OnMouseMove_RB(APoint: TFloatPoint);
begin

end;

{ THandTool }

procedure THandTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  Field.FShift.X += FTemp.X - APoint.X;
  Field.FShift.Y += FTemp.Y - APoint.Y;

end;

procedure THandTool.OnMouseClick_LB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
begin
  FTemp := APoint;
end;

procedure THandTool.OnMouseClick_RB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.OnMouseMove_RB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.OnMouseClick_LB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(
                         APoint,APenColor,ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure TRoundRectangleTool.OnMouseClick_RB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(
                         APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

function TRoundRectangleTool.GetFigureProp: integer;
var
  f: TFigure;
begin
  f := TRoundRectangle.Create(Field.SceneToField(Point(0,0)), clWhite, clWhite);
  Result := GetPropList(f, PropList);
  f.Free;
end;

{ TEllipseTool }

procedure TEllipseTool.OnMouseClick_LB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure TEllipseTool.OnMouseClick_RB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint,APenColor,ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

function TEllipseTool.GetFigureProp: integer;
var
  f: TFigure;
begin
  f := TEllipse.Create(Field.SceneToField(Point(0,0)), clWhite, clWhite);
  Result := GetPropList(f, PropList);
  f.Free;
end;

{ TRectangleTool }

procedure TRectangleTool.OnMouseClick_LB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure TRectangleTool.OnMouseClick_RB(APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

function TRectangleTool.GetFigureProp: integer;
var
  f: TFigure;
begin
  f := TRectangle.Create(Field.SceneToField(Point(0,0)), clWhite, clWhite);
  Result := GetPropList(f, PropList);
  f.Free;
end;

{ TLineTool }

procedure TLineTool.OnMouseClick_LB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure TLineTool.OnMouseClick_RB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

function TLineTool.GetFigureProp: integer;
var
  f: TFigure;
begin
  f := TLine.Create(Field.SceneToField(Point(0,0)), clWhite, clWhite);
  Result := GetPropList(f, PropList);
  f.Free;
end;

{ TPencilTool }

procedure TPencilTool.OnMouseClick_LB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure TPencilTool.OnMouseClick_RB(APoint: TFloatPoint; APenColor: TColor;
  ABrushColor: TColor);
var
  i: integer;
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint,APenColor, ABrushColor);
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].isShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

function TPencilTool.GetFigureProp: integer;
var
  f: TFigure;
begin
  f := TPencil.Create(Field.SceneToField(Point(0,0)), clWhite, clWhite);
  Result := GetPropList(f, PropList);
  f.Free;
end;

 { TTool }

procedure TTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  Figures[High(Figures)].AddPoint(APoint);
end;

procedure TTool.OnMouseMove_RB(APoint: TFloatPoint);
begin
  Figures[High(Figures)].AddPoint(APoint);
end;

procedure RegisterTool(ATool: TTools);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := ATool.Create();
end;

constructor TTool.Create();
begin
  FIcon := TBitmap.Create;
end;

initialization
  RegisterTool(TPencilTool);
  RegisterTool(TLineTool);
  RegisterTool(TRectangleTool);
  RegisterTool(TEllipseTool);
  RegisterTool(TRoundRectangleTool);
  RegisterTool(THandTool);
  RegisterTool(TMagnifierTool);
end.

