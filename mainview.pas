unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, CheckLst, ColorBox, ValEdit, Spin, UTools, UFigures,
  UField,UFloatPoint, USpecialTools, LCLType;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    InfPanel: TPanel;
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
    procedure ChangeSpecialTool(Sender: TObject);
    procedure StyleBoxChange(Sender: TObject);
    procedure ZoomSpinChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurToolIndex: integer;
  DeletedInRow: integer;
  PaintStatus: (Draw, Act);
  CurSpecialToolIndex: integer;

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
  x := 5;
  y := 5;
  for i := 0 to High(Tools) do
    begin
      newButton := TSpeedButton.Create(InfPanel);
      newButton.Parent := InfPanel;
      newButton.Left := x;
      newButton.Top := y;
      newButton.Width := 40;
      newButton.Height := 40;
      newButton.Tag := (i + 1) * (-1);
      //newButton.Glyph := Tools[i].FIcon;
      newButton.OnClick := @ChangeSpecialTool;
      if x < 50 then
          x += 45
    end;
  ZoomSpin.Value := 100;
  Field.FZoom := ZoomSpin.Value / 100;
  ZoomValueLabel.Caption := FloatToStr(ZoomSpin.Value) + '%';
  PaintStatus := Draw;
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
  Field.FCenter := Point(Scene.Canvas.Width div 2, Scene.Canvas.Height div 2) ;
  Field.FFloatCenter := Field.SceneToField(Point(Scene.Canvas.Width div 2, Scene.Canvas.Height div 2)) ;
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
      if PaintStatus = Draw then
        begin
            begin
              Tools[CurToolIndex].StartDrawing(Field.SceneToField(Point(X, Y)),Scene.Canvas.Pen.Width,
                                                           Scene.Canvas.Pen.Style);
              Invalidate;
            end;
        end;
      if PaintStatus = Act then
        begin
           SpecialTools[CurSpecialToolIndex].Act(Field.SceneToField(Point(X,Y)));
           Invalidate;
        end;
    end;
end;

procedure TMainWindow.SceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    begin
      if PaintStatus = Draw then
        begin

            begin
              Tools[CurToolIndex].ContinueDrawing(Field.SceneToField(Point(X, Y)));
              Invalidate;
            end;
        end;
      if PaintStatus = Act then
        begin
          SpecialTools[CurSpecialToolIndex].ContinueAct(Field.SceneToField(Point(X, Y)));
          Invalidate;
        end;
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
  PaintStatus := Draw;
end;

procedure TMainWindow.ChangeSpecialTool(Sender: TObject);
begin
  CurSpecialToolIndex := (Sender as TSpeedButton).Tag;
  PaintStatus := Act;
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

procedure TMainWindow.ZoomSpinChange(Sender: TObject);
begin
  Field.FZoom := ZoomSpin.Value / 100;
  ZoomValueLabel.Caption := FloatToStr(ZoomSpin.Value) + '%';
  Invalidate;
end;

end.

