unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UFloatPoint, UField, TypInfo,
  ExtCtrls, StdCtrls, UEditors, FPCanvas;

type

   TTools = class of TTool;
   ToAddToHistory = procedure of object;
  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create(AFigures: TFigures);
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;
          APenColor: TColor; ABrushColor: TColor); virtual; abstract;
    procedure OnMouseMove_LB(Shift: TShiftState;APoint: TFloatPoint); virtual;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);
                                                  virtual; abstract;
    procedure OnMouseMove_RB(Shift: TShiftState;APoint: TFloatPoint); virtual;
    procedure OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint; APanel: TPanel); virtual;
    function GetFigureProp(): integer;virtual; abstract;
    public
      FFigure: TFigures;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer; override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer;override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor); override;
    function GetFigureProp(): integer;override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    function GetFigureProp: integer; override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    function GetFigureProp: integer; override;
  end;


  { TPositioningTool }

  TPositioningTool = class(TTool)
    procedure OnMouseMove_LB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(Shift: TShiftState;APoint: TFloatPoint); override;
  end;


  { THandTool }

  THandTool = class(TPositioningTool)
    procedure OnMouseMove_LB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
      APanel: TPanel); override;
    function GetFigureProp: integer; override;
    public
      FTemp: TFloatPoint;
  end;


  { TMoveSelectedFiguresTool }

  TMoveSelectedFiguresTool = class(TPositioningTool)
    procedure OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseMove_LB(Shift: TShiftState; APoint: TFloatPoint); override;
    procedure OnMouseMove_RB(Shift: TShiftState; APoint: TFloatPoint); override;
    function GetFigureProp: integer; override;
  private
    FTempPoint: TFloatPoint;
    FVector: TFloatPoint;
  end;


  { TMagnifierTool }

  TMagnifierTool = class(TPositioningTool)
    procedure OnMouseMove_LB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint;APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseMove_RB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
      APanel: TPanel); override;
    function GetFigureProp: integer; override;
  end;


  procedure RegisterTool(ATool: TTools; AFigures: TFigures);
  procedure ChangeProps;
  procedure UnSelect;

 var
   Tools: array of TTool;
   Editors: array of TEditor;
   PropList: PPropList;
   AddFigureToHistory: ToAddToHistory;

implementation

{ TMoveSelectedFiguresTool }

procedure TMoveSelectedFiguresTool.OnMouseClick_LB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  FTempPoint := APoint;
end;

procedure TMoveSelectedFiguresTool.OnMouseClick_RB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  inherited OnMouseClick_RB(Shift, APoint, APenColor, ABrushColor);
end;

procedure TMoveSelectedFiguresTool.OnMouseMove_LB(Shift: TShiftState;
  APoint: TFloatPoint);
var
  i: integer;
  lgth: Double;
  vector: TFloatPoint;
begin
  Vector.X += APoint.X - FTempPoint.X;
  Vector.Y += APoint.Y - FTempPoint.Y;
  For i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        Figures[i].MoveFigure(Vector);
    end;
  FTempPoint := APoint;
end;

procedure TMoveSelectedFiguresTool.OnMouseMove_RB(Shift: TShiftState;
  APoint: TFloatPoint);
begin
  inherited OnMouseMove_RB(Shift, APoint);
end;

function TMoveSelectedFiguresTool.GetFigureProp: integer;
begin
  Result := 0;
end;

{ TMagnifierTool }

procedure TMagnifierTool.OnMouseMove_LB(Shift: TShiftState; APoint: TFloatPoint
  );
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseClick_LB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
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

procedure TMagnifierTool.OnMouseClick_RB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
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

procedure TMagnifierTool.OnMouseMove_RB(Shift: TShiftState; APoint: TFloatPoint
  );
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
  APanel: TPanel);
begin

end;

function TMagnifierTool.GetFigureProp: integer;
begin
  Result := 0;
end;

{ TPositioningTool }

procedure TPositioningTool.OnMouseMove_LB(Shift: TShiftState;
  APoint: TFloatPoint);
begin

end;

procedure TPositioningTool.OnMouseClick_LB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TPositioningTool.OnMouseClick_RB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TPositioningTool.OnMouseMove_RB(Shift: TShiftState;
  APoint: TFloatPoint);
begin

end;

{ THandTool }

procedure THandTool.OnMouseMove_LB(Shift: TShiftState; APoint: TFloatPoint);
begin
  Field.FShift.X += FTemp.X - APoint.X;
  Field.FShift.Y += FTemp.Y - APoint.Y;

end;

procedure THandTool.OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  FTemp := APoint;
end;

procedure THandTool.OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.OnMouseMove_RB(Shift: TShiftState; APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
  APanel: TPanel);
begin

end;

function THandTool.GetFigureProp: integer;
begin
  Result := 0;
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.OnMouseClick_LB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(
                         APoint,APenColor,ABrushColor);
  ChangeProps;
end;

procedure TRoundRectangleTool.OnMouseClick_RB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(
                         APoint, APenColor, ABrushColor);
  ChangeProps;
end;

function TRoundRectangleTool.GetFigureProp: integer;
begin
  Result := GetPropList(TRoundRectangle, PropList);
end;

{ TEllipseTool }

procedure TEllipseTool.OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

procedure TEllipseTool.OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint,APenColor,ABrushColor);
  ChangeProps;
end;

function TEllipseTool.GetFigureProp: integer;
begin
  Result := GetPropList(TEllipse, PropList);
end;

{ TRectangleTool }

procedure TRectangleTool.OnMouseClick_LB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

procedure TRectangleTool.OnMouseClick_RB(Shift: TShiftState;
  APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

function TRectangleTool.GetFigureProp: integer;
begin
  Result := GetPropList(TRectangle, PropList);
end;

{ TLineTool }

procedure TLineTool.OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

procedure TLineTool.OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

function TLineTool.GetFigureProp: integer;
begin
  Result := GetPropList(TLine, PropList);
end;

{ TPencilTool }

procedure TPencilTool.OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, APenColor, ABrushColor);
  ChangeProps;
end;

procedure TPencilTool.OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin
  UnSelect;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint,APenColor, ABrushColor);
  ChangeProps;
end;

function TPencilTool.GetFigureProp: integer;
begin
  Result := GetPropList(TPencil, PropList);
end;

 { TTool }

procedure TTool.OnMouseMove_LB(Shift: TShiftState; APoint: TFloatPoint);
begin
  Figures[High(Figures)].AddPoint(APoint);
end;

procedure TTool.OnMouseMove_RB(Shift: TShiftState; APoint: TFloatPoint);
begin
  Figures[High(Figures)].AddPoint(APoint);
end;

procedure TTool.OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
  APanel: TPanel);
begin
  AddFigureToHistory;
end;

procedure RegisterTool(ATool: TTools; AFigures : TFigures);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := ATool.Create(AFigures);
end;

procedure ChangeProps;
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    if RegisteredEditors[i].FIsShown then
    SetPropValue(Figures[High(Figures)], RegisteredEditors[i].FName,
                                         RegisteredEditors[i].FValue);
end;

procedure UnSelect;
var
  i: integer;
begin
  for i := 0 to High(Figures)do
    if Figures[i].isSelected then
      Figures[i].isSelected := False;
  ToRepaint;
end;

constructor TTool.Create(AFigures: TFigures);
begin
  FIcon := TBitmap.Create;
  FFigure := AFigures;
end;

initialization
  RegisterTool(TPencilTool, TPencil);
  RegisterTool(TLineTool, TLine);
  RegisterTool(TRectangleTool, TRectangle);
  RegisterTool(TEllipseTool, TEllipse);
  RegisterTool(TRoundRectangleTool, TRoundRectangle);
  RegisterTool(THandTool, TFigure);
  RegisterTool(TMagnifierTool, TFigure);
  RegisterTool(TMoveSelectedFiguresTool, TFigure);
end.

