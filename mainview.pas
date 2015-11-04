unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls,Spin, UTools, UFigures,
  UField,UFloatPoint, LCLType, Grids;

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
    ZoomSpin: TFloatSpinEdit;
    ZoomLabel: TLabel;
    StyleBox: TComboBox;
    StyleLabel: TLabel;
    WidthSpin: TSpinEdit;
    WidthLabel: TLabel;
    PropertiesLabel: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    FileExit: TMenuItem;
    HelpAbout: TMenuItem;
    ManuDrawing: TMenuItem;
    DrawingClear: TMenuItem;
    PropertiesPanel: TPanel;
    Scene: TPaintBox;
    ToolsPanel: TPanel;
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
    procedure StyleBoxChange(Sender: TObject);
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure VerticalScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ZoomSpinChange(Sender: TObject);
    procedure OnChange;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurToolIndex: integer;
  CurColorIndex: integer;
  DeletedInRow: integer;
  MaxX: Integer;
  MaxY: Integer;
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
      newButton.Glyph := Tools[i].FIcon;
      newButton.OnClick := @ChangeTool;
      if x < 50 then
          x += 45
          else
            begin
              x := 5;
              y += 45;
            end;
    end;
  ZoomSpin.Value := 100;
  Field.FZoom := ZoomSpin.Value / 100;
  ZoomValueLabel.Caption := FloatToStr(ZoomSpin.Value) + '%';
  VerticalScrollBar.Min := 0;
  VerticalScrollBar.Max := 0;
  VerticalScrollBar.Position := 0;
  HorizontalScrollBar.Min := 0;
  HorizontalScrollBar.Max := 0;
  HorizontalScrollBar.Position := 0;
  MaxY := Scene.Height;
  MaxX := Scene.Width;
  MainColor.Tag := 0;
  AdditionalColor.Tag := 1;
  CurColorIndex := 0;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if (Key = VK_0) then
     // begin
        //if (Length(Figures) <> 0) and (DeletedInRow <> 9) then
           // begin
             // SetLength(Figures, 0);
             // Invalidate;
           // end;
     // end;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  if Scene.Height > MaxY then
    MaxY := Scene.Height;
  if Scene.Width > MaxX then
    MaxX := Scene.Width;
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
end;

procedure TMainWindow.AdditionalColorDblClick(Sender: TObject);
begin
  CurColorIndex := 1;
  ColorDialog.Execute;
  AdditionalColor.Color := ColorDialog.Color;
end;

procedure TMainWindow.HelpAboutClick(Sender: TObject);
begin
  ShowMessage('Терехов Дмитрий, Б8103а, 2015 - 2016');
end;

procedure TMainWindow.HorizontalScrollBarChange(Sender: TObject);
begin
  Field.FScrollBarShift.X := HorizontalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.InfPanelClick(Sender: TObject);
begin

end;

procedure TMainWindow.MainColorClick(Sender: TObject);
begin
  CurColorIndex := 0;
end;

procedure TMainWindow.MainColorDblClick(Sender: TObject);
begin
  CurColorIndex := 0;
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
  Scene.Canvas.Pen.Width := WidthSpin.Value;
  Scene.Canvas.Pen.Color := MainColor.Color;
  Scene.Canvas.Brush.Color := AdditionalColor.Color;
  if ssLeft in Shift then
    begin
      Scene.Canvas.Pen.Width := WidthSpin.Value;
      Tools[CurToolIndex].OnMouseClick_LB(Field.SceneToField(Point(X, Y)),Scene.Canvas.Pen.Width,
                                                                          Scene.Canvas.Pen.Style,
                                                                          Scene.Canvas.Pen.Color,
                                                                          Scene.Canvas.Brush.Color);

      OnChange;
      Invalidate;
    end;
  Scene.Canvas.Pen.Color := AdditionalColor.Color;
  Scene.Canvas.Brush.Color := MainColor.Color;
  if ssRight in Shift then
    begin
      Scene.Canvas.Pen.Width := WidthSpin.Value;
      Tools[CurToolIndex].OnMouseClick_RB(Field.SceneToField(Point(X,Y)), Scene.Canvas.Pen.Width,
                                                                          Scene.Canvas.Pen.Style,
                                                                          Scene.Canvas.Pen.Color,
                                                                          Scene.Canvas.Brush.Color);
      OnChange;
      Invalidate;
    end;
end;

procedure TMainWindow.SceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Scene.Canvas.Pen.Width := WidthSpin.Value;
  if ssLeft in Shift then
    begin
      Tools[CurToolIndex].OnMouseMove_LB(Field.SceneToField(Point(X, Y)));
      if Y > MaxY then
         begin
           VerticalScrollBar.Visible := true;
           VerticalScrollBar.Enabled := true;
           MaxY := Y;
           VerticalScrollBar.SetParams(VerticalScrollBar.Position, 0, VerticalScrollBar.Max + 2);
         end;
      if X > MaxX then
        begin
           HorizontalScrollBar.Visible := true;
           HorizontalScrollBar.Enabled := true;
           MaxX := X;
           HorizontalScrollBar.SetParams(HorizontalScrollBar.Position, 0, HorizontalScrollBar.Max + 2);
        end;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
      Tools[CurToolIndex].OnMouseMove_RB(Field.SceneToField(Point(X, Y)));
      if Y > MaxY then
         begin
           VerticalScrollBar.Visible := true;
           VerticalScrollBar.Enabled := true;
           MaxY := Y;
           VerticalScrollBar.SetParams(VerticalScrollBar.Position, 0, VerticalScrollBar.Max + 2);
         end;
      if X > MaxX then
        begin
           HorizontalScrollBar.Visible := true;
           HorizontalScrollBar.Enabled := true;
           MaxX := X;
           HorizontalScrollBar.SetParams(HorizontalScrollBar.Position, 0, HorizontalScrollBar.Max + 2);
        end;
      Invalidate;
    end;
end;

procedure TMainWindow.ScenePaint(Sender: TObject);
var i: TFigure;
begin
  for i in Figures do
    i.Draw(Scene.Canvas);
end;

procedure TMainWindow.ChangeTool(Sender: TObject);
begin
  CurToolIndex := (Sender as TSpeedButton).Tag;
end;


procedure TMainWindow.StyleBoxChange(Sender: TObject);
begin
  case StyleBox.Text of
    'Solid': Scene.Canvas.Pen.Style := psSolid;
    'Dash': Scene.Canvas.Pen.Style := psDash;
    'Dot': Scene.Canvas.Pen.Style := psDot;
    'DashDot': Scene.Canvas.Pen.Style := psDashDot;
    'DashDotDot': Scene.Canvas.Pen.Style := psDashDotDot;
  end;
end;

procedure TMainWindow.VerticalScrollBarChange(Sender: TObject);
begin
  Field.FScrollBarShift.Y := VerticalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.VerticalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TMainWindow.ZoomSpinChange(Sender: TObject);
begin
  Field.FZoom := ZoomSpin.Value / 100;
  ZoomValueLabel.Caption := FloatToStr(ZoomSpin.Value) + '%';
  Invalidate;
end;

procedure TMainWindow.OnChange;
begin
  ZoomSpin.Value := Field.FZoom * 100;
  ZoomValueLabel.Caption := FloatToStr(ZoomSpin.Value) + '%';
  Field.FZoom := ZoomSpin.Value / 100;
  Invalidate;
end;

end.

