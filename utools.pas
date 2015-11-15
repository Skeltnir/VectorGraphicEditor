unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UFloatPoint, UField, TypInfo,ExtCtrls,
  StdCtrls;

type


  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create(AName: String);
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
                                                 APenColor: TColor; ABrushColor: TColor);
                                                             virtual; abstract;
    procedure OnMouseMove_LB(APoint: TFloatPoint); virtual;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
                                                 APenColor: TColor; ABrushColor: TColor); virtual; abstract;
    procedure OnMouseMove_RB(APoint: TFloatPoint); virtual;
    procedure GetInterface(APanel: TPanel); virtual; abstract;
    procedure OnWidthBoxChange(Sender: TObject); virtual;
    procedure OnStyleBoxChange(Sender: TObject); virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure GetInterface(APanel: TPanel); override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle:
      TPenStyle; APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor);override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


  { TPositioningTool }

  TPositioningTool = class(TTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ; APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


  { THandTool }

  THandTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ; APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
    procedure GetInterface(APanel: TPanel); override;
    public
      FTemp: TFloatPoint;
  end;


  { TMagnifierTool }

  TMagnifierTool = class(TPositioningTool)
    procedure OnMouseMove_LB(APoint: TFloatPoint); override;
    procedure OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle
      ; APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
      APenColor: TColor; ABrushColor: TColor); override;
    procedure OnMouseMove_RB(APoint: TFloatPoint); override;
    procedure GetInterface(APanel: TPanel); override;
  end;


 var
   Tools: array [0..6] of TTool;
   ComboBoxes: array of TComboBox;
   MainScene: TPaintBox;
implementation


{ TMagnifierTool }

procedure TMagnifierTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure TMagnifierTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
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

procedure TMagnifierTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle;
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

procedure TMagnifierTool.GetInterface(APanel: TPanel);
begin
  inherited GetInterface(APanel);
end;

{ TPositioningTool }

procedure TPositioningTool.OnMouseMove_LB(APoint: TFloatPoint);
begin

end;

procedure TPositioningTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TPositioningTool.OnMouseClick_RB(APoint: TFloatPoint;
  AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor
  );
begin

end;

procedure TPositioningTool.OnMouseMove_RB(APoint: TFloatPoint);
begin

end;

procedure TPositioningTool.GetInterface(APanel: TPanel);
begin

end;

{ THandTool }

procedure THandTool.OnMouseMove_LB(APoint: TFloatPoint);
begin
  Field.FShift.X += FTemp.X - APoint.X;
  Field.FShift.Y += FTemp.Y - APoint.Y;

end;

procedure THandTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  FTemp := APoint;
end;

procedure THandTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.OnMouseMove_RB(APoint: TFloatPoint);
begin
  InheritsFrom(TPositioningTool);
end;

procedure THandTool.GetInterface(APanel: TPanel);
begin
  inherited GetInterface(APanel);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer;APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TRoundRectangleTool.OnMouseClick_RB(APoint: TFloatPoint;
  AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor
  );
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TRoundRectangleTool.GetInterface(APanel: TPanel);
begin
  inherited;
end;

{ TEllipseTool }

procedure TEllipseTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TEllipseTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TEllipseTool.GetInterface(APanel: TPanel);
begin
  inherited;
end;

{ TRectangleTool }

procedure TRectangleTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TRectangleTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TRectangleTool.GetInterface(APanel: TPanel);
begin
  inherited;
end;

{ TLineTool }

procedure TLineTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TLineTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TLineTool.GetInterface(APanel: TPanel);
begin
    SetLength(ComboBoxes, Length(ComboBoxes) + 1);
  ComboBoxes[High(Comboboxes)] := TComboBox.Create(APanel);
  With ComboBoxes[High(ComboBoxes)] do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := 5;
      Top := 5;
      Parent := APanel;
      Enabled := True;
      AddItem('1', ComboBoxes[High(Comboboxes)]);
      AddItem('3', ComboBoxes[High(Comboboxes)]);
      AddItem('9', ComboBoxes[High(Comboboxes)]);
      AddItem('15', ComboBoxes[High(Comboboxes)]);
      ReadOnly := True;
      OnChange := @OnWidthBoxChange;
    end;
 SetLength(ComboBoxes, Length(ComboBoxes) + 1);
 ComboBoxes[High(Comboboxes)] := TComboBox.Create(APanel);
  With ComboBoxes[High(ComboBoxes)] do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := 5;
      Top := 50;
      Parent := APanel;
      Enabled := True;
      AddItem('Solid', ComboBoxes[High(Comboboxes)]);
      AddItem('Dash', ComboBoxes[High(Comboboxes)]);
      AddItem('Dot', ComboBoxes[High(Comboboxes)]);
      AddItem('DashDot', ComboBoxes[High(Comboboxes)]);
      AddItem('DashDotDot', ComboBoxes[High(Comboboxes)]);
      ReadOnly := True;
      OnChange := @OnStyleBoxChange;
    end;
end;

{ TPencilTool }

procedure TPencilTool.OnMouseClick_LB(APoint: TFloatPoint; AWidth: Integer; APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TPencilTool.OnMouseClick_RB(APoint: TFloatPoint; AWidth: Integer;
  APenStyle: TPenStyle; APenColor: TColor; ABrushColor: TColor);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, AWidth, APenStyle, APenColor, ABrushColor);
end;

procedure TPencilTool.GetInterface(APanel: TPanel);
begin
  SetLength(ComboBoxes, Length(ComboBoxes) + 1);
  ComboBoxes[High(Comboboxes)] := TComboBox.Create(APanel);
  With ComboBoxes[High(ComboBoxes)] do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := 5;
      Top := 5;
      Parent := APanel;
      Enabled := True;
      AddItem('1', ComboBoxes[High(Comboboxes)]);
      AddItem('3', ComboBoxes[High(Comboboxes)]);
      AddItem('9', ComboBoxes[High(Comboboxes)]);
      AddItem('15', ComboBoxes[High(Comboboxes)]);
      ReadOnly := True;
      OnChange := @OnWidthBoxChange;
    end;
 SetLength(ComboBoxes, Length(ComboBoxes) + 1);
 ComboBoxes[High(Comboboxes)] := TComboBox.Create(APanel);
  With ComboBoxes[High(ComboBoxes)] do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := 5;
      Top := 50;
      Parent := APanel;
      Enabled := True;
      AddItem('Solid', ComboBoxes[High(Comboboxes)]);
      AddItem('Dash', ComboBoxes[High(Comboboxes)]);
      AddItem('Dot', ComboBoxes[High(Comboboxes)]);
      AddItem('DashDot', ComboBoxes[High(Comboboxes)]);
      AddItem('DashDotDot', ComboBoxes[High(Comboboxes)]);
      ReadOnly := True;
      OnChange := @OnStyleBoxChange;
    end;
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

procedure TTool.OnWidthBoxChange(Sender: TObject);
begin
  Case (Sender as TComboBox).Text of
    '1': MainScene.Canvas.Pen.Width := 1;
    '3': MainScene.Canvas.Pen.Width := 3;
    '9': MainScene.Canvas.Pen.Width := 9;
    '15': MainScene.Canvas.Pen.Width := 15;
  end;
end;

procedure TTool.OnStyleBoxChange(Sender: TObject);
begin
  Case (Sender as TComboBox).Text of
    'Solid': MainScene.Canvas.Pen.Style := psSolid;
    'Dash': MainScene.Canvas.Pen.Style := psDash;
    'Dot': MainScene.Canvas.Pen.Style := psDot;
    'DashDot': MainScene.Canvas.Pen.Style := psDashDot;
    'DashDotDot': MainScene.Canvas.Pen.Style := psDashDotDot;
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

