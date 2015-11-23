unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls,Spin, UTools, UFigures,
  UField,UFloatPoint, LCLType, Grids, UEditors, typinfo;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    ColorDialog: TColorDialog;
    Palette: TDrawGrid;
    InfPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
    AdditionalColor: TPanel;
    MainColor: TPanel;
    VerticalScrollBar: TScrollBar;
    ZoomValueLabel: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    FileExit: TMenuItem;
    HelpAbout: TMenuItem;
    ManuDrawing: TMenuItem;
    DrawingClear: TMenuItem;
    ToolsPanel: TPanel;
    Scene: TPaintBox;
    PropertiesPanel: TPanel;
    procedure AdditionalColorClick(Sender: TObject);
    procedure AdditionalColorDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DrawingClearClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure InfPanelClick(Sender: TObject);
    procedure MainColorClick(Sender: TObject);
    procedure MainColorDblClick(Sender: TObject);
    procedure ManuDrawingClick(Sender: TObject);
    procedure PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ScenePaint(Sender: TObject);
    procedure ChangeTool(Sender: TObject);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure VerticalScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure OnChange;
    procedure BuildInterfaceOfProp(ANumber: integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurToolIndex: integer;
  CurColorIndex: integer;
  Borders: TFloatPoints;
  Colors: Array[0..4, 0..17] of TColor;



implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  newButton: TSpeedButton;
  i, x, y: integer;
begin
  CurToolIndex := 0;
  x := 5;
  y := 5;
  for i := 0 to High(Tools) do
    begin
      newButton := TSpeedButton.Create(ToolsPanel);
      newButton.Parent := ToolsPanel;
      newButton.Left := x;
      newButton.Top := y;
      newButton.Width := 40;
      newButton.Height := 40;
      newButton.Tag := i;
      Tools[i].FIcon.LoadFromFile('Images\' + IntToStr(i + 1) + '.bmp');
      newButton.Glyph := Tools[i].FIcon;
      newButton.OnClick := @ChangeTool;
      x += 45;
      if i = 0 then
        newButton.Click;
    end;
  VerticalScrollBar.Min := 0;
  VerticalScrollBar.Max := 0;
  VerticalScrollBar.Position := 0;
  HorizontalScrollBar.Min := 0;
  HorizontalScrollBar.Max := 0;
  HorizontalScrollBar.Position := 0;
  MainColor.Tag := 0;
  AdditionalColor.Tag := 1;
  CurColorIndex := 0;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
end;

procedure TMainWindow.FileExitClick(Sender: TObject);
begin
  MainWindow.Close;
end;

procedure TMainWindow.DrawingClearClick(Sender: TObject);
begin
  SetLength(Figures, 0);
  Invalidate;
end;

procedure TMainWindow.Button1Click(Sender: TObject);
begin

end;

procedure TMainWindow.AdditionalColorClick(Sender: TObject);
begin
  CurColorIndex := 1;
  AdditionalColor.BorderStyle := bsSingle;
  MainColor.BorderStyle := bsNone;
end;

procedure TMainWindow.AdditionalColorDblClick(Sender: TObject);
begin
  CurColorIndex := 1;
  AdditionalColor.BorderStyle := bsSingle;
  MainColor.BorderStyle := bsNone;
  ColorDialog.Execute;
  AdditionalColor.Color := ColorDialog.Color;
end;

procedure TMainWindow.HelpAboutClick(Sender: TObject);
begin
  ShowMessage('Терехов Дмитрий, Б8103а, 2015 - 2016');
end;

procedure TMainWindow.HorizontalScrollBarChange(Sender: TObject);
begin
  Field.FShift.X := HorizontalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.InfPanelClick(Sender: TObject);
begin

end;

procedure TMainWindow.MainColorClick(Sender: TObject);
begin
  CurColorIndex := 0;
  AdditionalColor.BorderStyle := bsNone;
  MainColor.BorderStyle := bsSingle;
end;

procedure TMainWindow.MainColorDblClick(Sender: TObject);
begin
  CurColorIndex := 0;
  AdditionalColor.BorderStyle := bsNone;
  MainColor.BorderStyle := bsSingle;
  ColorDialog.Execute;
  MainColor.Color := ColorDialog.Color;
end;

procedure TMainWindow.ManuDrawingClick(Sender: TObject);
begin

end;

procedure TMainWindow.PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  r,g,b,i : integer;
begin
  r := 240; g := 240; b := 240;
  for i := 15 downto 0 do
    begin
      if (aCol = i) and (aRow = 4) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       r -= 16;
       g -= 16;
       b -= 16;
    end;
  r := 0; g := 255; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
   r := 85; g := 255; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 255; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 255; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 170; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 170; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 170; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
   r := 255; g := 170; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 85; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 85; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 85; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 85; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 0; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 0; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 0; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 0; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
end;

procedure TMainWindow.PaletteSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  case CurColorIndex of
    0: MainColor.Color := Colors[aRow,aCol];
    1: AdditionalColor.Color := Colors[aRow,aCol];
  end;
end;

procedure TMainWindow.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Scene.Canvas.Pen.Color := MainColor.Color;
      Scene.Canvas.Brush.Color := AdditionalColor.Color;
      Tools[CurToolIndex].OnMouseClick_LB(Field.SceneToField(Point(X, Y)),
        Scene.Canvas.Pen.Color,
        Scene.Canvas.Brush.Color);
      OnChange;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
       Scene.Canvas.Pen.Color := AdditionalColor.Color;
       Scene.Canvas.Brush.Color := MainColor.Color;
      Tools[CurToolIndex].OnMouseClick_RB(
        Field.SceneToField(Point(X,Y)),
        Scene.Canvas.Pen.Color,
        Scene.Canvas.Brush.Color);
      OnChange;
      Invalidate;
    end;
end;

procedure TMainWindow.SceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Tools[CurToolIndex].OnMouseMove_LB(Field.SceneToField(Point(X, Y)));
      OnChange;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
      Tools[CurToolIndex].OnMouseMove_RB(Field.SceneToField(Point(X, Y)));
      OnChange;
      Invalidate;
    end;
end;

procedure TMainWindow.ScenePaint(Sender: TObject);
var
  i: TFigure;
  a, temp: TFloatPoints;
begin
  if Length(Figures) <> 0 then
    begin
      a[0].X := Figures[0].FPoints[0].X;
      a[1].X := Figures[0].FPoints[0].X;
      a[0].Y := Figures[0].FPoints[0].X;
      a[1].Y := Figures[0].FPoints[0].X;
      for i in Figures do
        begin
          i.Draw(Scene.Canvas);
          temp := i.GetBorders(i.FPoints);
          if a[0].X < temp[0].X then a[0].X := temp[0].X;
          if a[0].Y < temp[0].Y then a[0].Y := temp[0].Y;
          if a[1].X > temp[1].X then a[1].X := temp[1].X;
          if a[1].Y > temp[1].Y then a[1].Y := temp[1].Y;
        end;
      if a[0].X > Scene.Width then
        begin
          HorizontalScrollBar.Visible := True;
          HorizontalScrollBar.SetParams(HorizontalScrollBar.Position,
                                        HorizontalScrollBar.Min,
                                        round(a[0].X - Scene.Width));
        end;
      if a[0].Y > Scene.Height then
        begin
          VerticalScrollBar.Visible := True;
          VerticalScrollBar.SetParams(VerticalScrollBar.Position,
                                      VerticalScrollBar.Min,
                                      round(a[0].Y - Scene.Height));
        end;
      if a[1].X < 0 then
        begin
          HorizontalScrollBar.Visible := True;
          HorizontalScrollBar.SetParams(HorizontalScrollBar.Position,
                                        round(a[1].X),
                                        HorizontalScrollBar.Max);
        end;
      if a[1].Y < 0 then
        begin
          VerticalScrollBar.Visible := True;
          VerticalScrollBar.SetParams(VerticalScrollBar.Position,
                                      round(a[1].Y),
                                      VerticalScrollBar.Max);
        end;
    end;
end;

procedure TMainWindow.ChangeTool(Sender: TObject);
var
  i, n, j, y: integer;
begin
  y := 5 ;
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].isShown = True then
        RegisteredEditors[i].Hide;
    end;
  CurToolIndex := (Sender as TSpeedButton).Tag;
  n := Tools[CurToolIndex].GetFigureProp();
  for  i := 0 to n - 1 do
    for j := 0 to High(RegisteredEditors) do
      begin
        if PropList^[i]^.Name = RegisteredEditors[j].FName then
          begin
            RegisteredEditors[j].Show(PropertiesPanel, 80, 80,5, y );
            y += 45;
          end;

      end;
end;

procedure TMainWindow.VerticalScrollBarChange(Sender: TObject);
begin
  Field.FShift.Y := VerticalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.VerticalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TMainWindow.OnChange;
begin
  VerticalScrollBar.Position := round(Field.FShift.Y);
  HorizontalScrollBar.Position := round(Field.FShift.X);
  Invalidate;
end;

procedure TMainWindow.BuildInterfaceOfProp(ANumber: integer);
begin

end;

end.

