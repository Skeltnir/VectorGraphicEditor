unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls,Spin, UTools, UFigures,
  UField,UFloatPoint, LCLType;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    InfPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
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
    procedure Button1Click(Sender: TObject);
    procedure DrawingClearClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure InfPanelClick(Sender: TObject);
    procedure ManuDrawingClick(Sender: TObject);
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
  DeletedInRow: integer;
  MaxX: Integer;
  MaxY: Integer;

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
  //VerticalScrollBar.PageSize := Scene.Height;
  MaxY := Scene.Height;
  MaxX := Scene.Width;
  //VerticalScrollBar.Visible := true;
           //VerticalScrollBar.Enabled := true;

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

procedure TMainWindow.ManuDrawingClick(Sender: TObject);
begin

end;

procedure TMainWindow.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Scene.Canvas.Pen.Width := WidthSpin.Value;
      Tools[CurToolIndex].OnMouseClick_LB(Field.SceneToField(Point(X, Y)),Scene.Canvas.Pen.Width,
                                                                      Scene.Canvas.Pen.Style);
      OnChange;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
      Scene.Canvas.Pen.Width := WidthSpin.Value;
      Tools[CurToolIndex].OnMouseClick_RB(Field.SceneToField(Point(X,Y)));
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
      if Y > MaxY then
         begin
           VerticalScrollBar.Visible := true;
           VerticalScrollBar.Enabled := true;
           MaxY := Y;
           VerticalScrollBar.SetParams(VerticalScrollBar.Position, 0, VerticalScrollBar.Max + 1);
         end;
      if X > MaxX then
        begin
           HorizontalScrollBar.Visible := true;
           HorizontalScrollBar.Enabled := true;
           MaxX := X;
           HorizontalScrollBar.SetParams(HorizontalScrollBar.Position, 0, HorizontalScrollBar.Max + 1);
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

