unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UFloatPoint, UField, TypInfo,
  ExtCtrls, StdCtrls, UEditors, FPCanvas;

type


  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create(AName: String);
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
                                                 APenStyle: TFPPenStyle;
                                                 APenColor: TColor;
                                                 ABrushColor: TColor;
                                                 ABrushColorStatus: Boolean);
                                                             virtual; abstract;
    procedure OnMouseMove_LB(APoint: TFloatPoint); virtual;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
                                                 APenStyle: TFPPenStyle;
                                                 APenColor: TColor;
                                                 ABrushColor: TColor;
                                                 ABrushColorStatus: Boolean); virtual; abstract;
    procedure OnMouseMove_RB(APoint: TFloatPoint); virtual;
    procedure GetInterface(APanel: TPanel; AToolIndex: Integer); virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle:
      TFPPenStyle; APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
  end;


  { TPositioningTool }

  TPositioningTool = class(TTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle
      ; APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
  end;


  { THandTool }

  THandTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle
      ; APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
    public
      FTemp: TFloatPoint;
  end;


  { TMagnifierTool }

  TMagnifierTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle
      ; APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TFPPenStyle;
      APenColor: TColor; ABrushColor: TColor; ABrushColorStatus: Boolean); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
  end;




 var
   Tools: array [0..6] of TTool;
    Editors: array of TEditor;
    b: TPenWidth;
    c: TPenStyleEdt;
    d: TBrushColorStatus;
implementation

{ TMagnifierTool }

procedure TMagnifierTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
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

procedure TMagnifierTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
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
  AWidth: Integer; APenStyle: TFPPenStyle; APenColor: TColor;
  ABrushColor: TColor; ABrushColorStatus: Boolean);
begin

end;

procedure TPositioningTool.OnMouseClick_RB(APoint: TFloatPoint;
  AWidth: Integer; APenStyle: TFPPenStyle; APenColor: TColor;
  ABrushColor: TColor; ABrushColorStatus: Boolean);
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

procedure THandTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  FTemp := APoint;
end;

procedure THandTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.OnMouseMove_RB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.OnMouseClick_LB(APoint: TFloatPoint;
  AWidth: Integer; APenStyle: TFPPenStyle; APenColor: TColor;
  ABrushColor: TColor; ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, AWidth,
                                                           APenStyle,
                                                           APenColor,
                                                           ABrushColor,
                                                           ABrushColorStatus);
end;

procedure TRoundRectangleTool.OnMouseClick_RB(APoint: TFloatPoint;
  AWidth: Integer; APenStyle: TFPPenStyle; APenColor: TColor;
  ABrushColor: TColor; ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, AWidth,
                                                           APenStyle,
                                                           APenColor,
                                                           ABrushColor,
                                                           ABrushColorStatus);
end;

{ TEllipseTool }

procedure TEllipseTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, AWidth,
                                                    APenStyle,
                                                    APenColor,
                                                    ABrushColor,
                                                    ABrushColorStatus);
end;

procedure TEllipseTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, AWidth,
                                                    APenStyle,
                                                    APenColor,
                                                    ABrushColor,
                                                    ABrushColorStatus);
end;

{ TRectangleTool }

procedure TRectangleTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, AWidth,
                                                      APenStyle,
                                                      APenColor,
                                                      ABrushColor,
                                                      ABrushColorStatus);
end;

procedure TRectangleTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, AWidth,
                                                      APenStyle,
                                                      APenColor,
                                                      ABrushColor,
                                                      ABrushColorStatus);
end;

{ TLineTool }

procedure TLineTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, AWidth,
                                                 APenStyle,
                                                 APenColor);
end;

procedure TLineTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, AWidth,
                                                 APenStyle,
                                                 APenColor);
end;

{ TPencilTool }

procedure TPencilTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, AWidth,
                                                   APenStyle,
                                                   APenColor);
end;

procedure TPencilTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TFPPenStyle; APenColor: TColor; ABrushColor: TColor;
  ABrushColorStatus: Boolean);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, AWidth,
                                                   APenStyle,
                                                   APenColor);
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

procedure TTool.GetInterface(APanel: TPanel; AToolIndex: Integer);
var
  y, i, PropNumber: integer;
  a: PPropList;
  Figure: TFigure;
begin
  if Length(Editors) <> 0 then
    begin
      for i := 0 to High(Editors) do
        begin
          Editors[i].Free;
        end;
    end;
  SetLength(Editors, 0);
  //b.Free;
 // c.Free;
  //d.Free;
  if AToolIndex < 5 then
    begin
      y := 5;
      case AToolIndex of
        0: Figure := TPencil.Create(Field.SceneToField(Point(0,0)),1,psSolid,clNone);
        1: Figure := TLine.Create(Field.SceneToField(Point(0,0)),1,psSolid,clNone);
        2: Figure := TRectangle.Create(Field.SceneToField(Point(0,0)),1,psSolid,clNone,clNone,False);
        3: Figure := TEllipse.Create(Field.SceneToField(Point(0,0)),1,psSolid,clNone,clNone,False);
        4: Figure := TRoundRectangle.Create(Field.SceneToField(Point(0,0)),1,psSolid,clNone,clNone,False);
      end;
      PropNumber := GetPropList(Figure,a);
      for i := 0 to PropNumber - 1 do
        begin
          if (a^[i]^.PropType^.Name = 'LongInt') then
            begin
              SetLength(Editors, Length(Editors) + 1);
              //b := TPenWidth.Create(APanel,5,y);
              Editors[High(Editors)] := TPenWidth.Create(APanel,5,y);
              y += 30
            end;
          if a^[i]^.PropType^.Name = 'TFPPenStyle' then
            begin
              SetLength(Editors, Length(Editors) + 1);
              //c := TPenStyleEdt.Create(APanel,5,y);
              Editors[High(Editors)] := TPenStyleEdt.Create(APanel,5,y);
              y += 30;
            end;
          if a^[i]^.PropType^.Name = 'Boolean' then
            begin
              SetLength(Editors, Length(Editors) + 1);
              //d := TBrushColorStatus.Create(APanel,5,y);
              Editors[High(Editors)] := TBrushColorStatus.Create(APanel,5,y);
              y += 30;
            end;
        end;
    end;
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

