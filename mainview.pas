unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, UTools, UFigures;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    FileExit: TMenuItem;
    HelpAbout: TMenuItem;
    ManuDrawing: TMenuItem;
    DrawingClear: TMenuItem;
    Scene: TPaintBox;
    ToolsPanel: TPanel;
    procedure DrawingClearClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure ManuDrawingClick(Sender: TObject);
    procedure SceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ScenePaint(Sender: TObject);
    procedure ChangeTool(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurToolIndex: Integer;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  newButton: TSpeedButton;
  i, x, y: integer;
begin
  CurToolIndex := 0;
  x:= 5;
  y:= 5;
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

procedure TMainWindow.HelpAboutClick(Sender: TObject);
begin
  ShowMessage('Терехов Дмитрий, Б8103а, 2015 - 2016');
end;

procedure TMainWindow.ManuDrawingClick(Sender: TObject);
begin

end;

procedure TMainWindow.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Tools[CurToolIndex].StartDrawing(Point(X, Y));
      Invalidate;
    end;
end;

procedure TMainWindow.SceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Tools[CurToolIndex].ContinueDrawing(Point(X, Y));
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

end.

